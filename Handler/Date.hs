module Handler.Date where

import Import
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Maybe
import Data.List (groupBy)
import Database.Persist.Query.Join (SelectOneMany (..), selectOneMany)
import Database.Persist.Query.Join.Sql (runJoin)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Sequence as Sq
import Data.Time.Clock
import System.Random

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

--look in views for what data is needed
--dateHandler :: GHandler s m (Day, [,Int], Bool, Int, 

getDateR :: Handler RepHtml
getDateR = do
   day <- liftIO getDay
   {-joinedDeliverables <- runDB $ runJoin (selectOneMany (CommentParent <-.) commentParent) 
      { somOrderOne  = [Asc DeliverableDueDate]
      , somFilterOne = [DeliverableDueDate >=. day, DeliverableDueDate <=. (addDays 6 day), DeliverableDeleted !=. True]
      , somIncludeNoMatch = True
      }
   -}
   deliverables <- runDB $ selectList [DeliverableDueDate >=. day,
                                       DeliverableDueDate <=. (addDays 6 day),
                                       DeliverableDeleted !=. True]
                                      [Asc DeliverableDueDate]
   let --deliverables = map (\ (x,y) -> x) joinedDeliverables
       dels = addNothings day $ splitDels deliverables
       nextWeek = nextFiveDays day
       dayNums = map gpm nextWeek
       elems = zip3 dels nextWeek $ zip dayNums ([1..7] :: [Int])
       goLeft = False
       week = 0
   (fWidget, enctype) <- generateFormPost assignmentForm
   defaultLayout $ do
      $(widgetFile "week")

getHorribleDuplicateR :: Int -> Handler RepHtml
getHorribleDuplicateR weeks = do
   dayOrigin <- liftIO getDay
   let day = (addDays (7 * fromIntegral weeks) dayOrigin)
   deliverables <- runDB $ selectList [DeliverableDueDate >=. day,
                                       DeliverableDueDate <=. (addDays 6 day),
                                       DeliverableDeleted !=. True]
                                      [Asc DeliverableDueDate]
   let dels = addNothings day $ splitDels deliverables
       nextWeek = nextFiveDays day
       dayNums = map gpm nextWeek
       elems = zip3 dels nextWeek $ zip dayNums ([1..7] :: [Int])
       week = weeks
       goLeft = if week > 0 then True else False
   (fWidget, enctype) <- generateFormPost assignmentForm
   defaultLayout $ do
      $(widgetFile "week")

getMonthR :: Handler RepHtml
getMonthR = do
   today <- liftIO getDay
   let (month@(firstDay:_), lastDay) = getMonth today
       validDays = frontPadding firstDay
                ++ (zip month $ repeat True)
                ++ backPadding lastDay
       weeks = getWeeks validDays
       dayStyle :: Day -> Text
       dayStyle theDay | theDay == today = "today" 
                       | not $ weekDay theDay = "weekend"
                       | otherwise = ""
       apostrophize dateText = T.cons '\'' dateText
       offset = 0
       neg = False
       makePlural :: Int -> Text
       makePlural cc = if cc == 1 then "" else "s"
   deliverables <- runDB $ selectList [DeliverableDueDate >=. firstDay,
                                    DeliverableDueDate <=. lastDay,
                                    DeliverableDeleted !=. True]
                                   [Asc DeliverableDueDate]
   let groupedDels = splitDels deliverables
   (fWidget, enctype) <- generateFormPost assignmentForm
   defaultLayout $ do
      $(widgetFile "month")

    
getMonthDupR :: Text -> Int -> Handler RepHtml
getMonthDupR isNeg offset = do
   today <- liftIO getDay
   let (month@(firstDay:_), lastDay) = getMonth $ addGregorianMonthsClip trueOffset today
       validDays = frontPadding firstDay
                ++ (zip month $ repeat True)
                ++ backPadding lastDay
       weeks = getWeeks validDays
       dayStyle :: Day -> Text
       dayStyle theDay | theDay == today = "today" 
                       | not $ weekDay theDay = "weekend"
                       | otherwise = ""
       apostrophize dateText = T.cons '\'' dateText
       neg = if isNeg == "n" then True else False
       makePlural :: Int -> Text
       makePlural cc = if cc == 1 then "" else "s"
       trueOffset :: Integer
       trueOffset = if neg then negate (toInteger offset) else toInteger offset
   deliverables <- runDB $ selectList [DeliverableDueDate >=. firstDay,
                                    DeliverableDueDate <=. lastDay,
                                    DeliverableDeleted !=. True]
                                   [Asc DeliverableDueDate]
   let groupedDels = splitDels deliverables
   (fWidget, enctype) <- generateFormPost assignmentForm
   defaultLayout $ do
      $(widgetFile "month")

postAssignmentR :: Text -> Text -> Text -> Int -> Handler RepHtml
postAssignmentR dayText monthOrWeek neg offset = do
   let day = (readT dayText :: Day)
   ((res, fWidget), enctype) <- runFormPost assignmentForm
   case res of
      FormSuccess assign@(DAssignment _ _ _ p) -> do
         let deliverable = validateAssignment day assign
         if (p == entryCode && deliverable /= Nothing)
            then do deliverableId <- runDB $ insert $ fromJust deliverable 
                    redirect $ if monthOrWeek == "week" then if offset == 0 then DateR
                                                                            else HorribleDuplicateR offset
                                                        else if offset == 0 then MonthR
                                                                            else MonthDupR neg offset
            else defaultLayout $ do --redirect!!!
                        [whamlet|<p>Improper Validation|]


postDeleteR :: DeliverableId -> Text -> Int -> Handler RepPlain
postDeleteR id weekOrMonth redirectPage = do
   runDB $ update id [DeliverableDeleted =. True]
   return $ RepPlain "Success"

getRestoreR :: Text -> Handler RepHtml
getRestoreR pw = do
   if pw == "restorePW0114" then do
      runDB $ updateWhere [DeliverableDeleted ==. True] [DeliverableDeleted =. False]
      redirect DateR
                            else do
      defaultLayout $ do --redirect!!!
                 [whamlet|<p>Don't hack me bro|]
getMigrateR :: Handler RepPlain
getMigrateR = do
   assignments <- runDB $ selectList [DeliverableDeleted !=. True]
                                     [Asc DeliverableDueDate]
   let display = T.unlines $ map (\ (Entity _ (Deliverable s a d day _ count)) -> showDel s a d day count) assignments
   return $ RepPlain $ toContent display
      where app = T.append
            delim = "<(|||)>"
            showDel s a d day count = showT s `app` delim `app` showT a 
                                `app` delim `app` showT d `app` delim `app` showT day `app` showT count

getUnmigrateR :: Handler RepHtml
getUnmigrateR = do
   defaultLayout $ do 
   [whamlet|
      <form method=post action=@{UnmigrateR}>
         <textarea name=assignments>
         <input type=submit>|]

postUnmigrateR :: Handler RepHtml
postUnmigrateR = do
   assignmentText <- runInputPost $ ireq textareaField "assignments"
   let textlines = T.lines $ unTextarea assignmentText
       delList = map (\ [s,a,d,day, count] -> (Deliverable (readT s) 
                                                    (readT a) 
                                                    (readT d)
                                                    (readT day)
                                                    False)
                                                    (readT count))
                 $ map (T.splitOn "<(|||)>") textlines
   mapM_ (runDB . insert) delList
   defaultLayout $ do
    [whamlet|
    $forall del <- delList
      #{show del} |]
      

getSummaryR :: Handler RepHtml
getSummaryR = do
   (passwidget, enctype) <- generateFormPost getLinkForm
   defaultLayout $ do [whamlet|
<form method=post enctype=enctype>
   ^{passwidget}
   <input type=submit> |]

postSummaryR :: Handler RepHtml
postSummaryR = do
   ((result, passwidget), enctype) <- runFormPost getLinkForm
   case result of
      FormSuccess _ -> do
         (redirect ("https://docs.google.com/spreadsheet/ccc?key=0AtkrCCC5nMmsdHpTYmZCeG5CMnd6Y3RpVHl0dEN1bUE&usp=sharing" :: Text) :: GHandler sub master a)
      _ -> do
         setMessage "Wrong password"
         redirect SummaryR

getViewAssignmentR :: DeliverableId -> Handler RepHtml
getViewAssignmentR assignId = do
   ((Entity _ (Deliverable s a d day _ _)), comments) : _ <- runDB $ runJoin (selectOneMany (CommentParent <-.) commentParent) 
      { somFilterOne = [DeliverableId ==. assignId]
      , somIncludeNoMatch = True
      , somOrderMany = [Asc CommentCreated]
      }
   (commentWidget, enctype) <- generateFormPost commentForm
   signature <- lookupSession "signature"
   defaultLayout $ do 
      $(widgetFile "comments")
      $(widgetFile "viewAssignment")      

postCommentR :: DeliverableId -> Handler RepHtml
postCommentR assignId = do
   ((result, commentWidget), enctype) <- runFormPost commentForm
   case result of
      FormSuccess (content, _) -> do
         signature <- lookupSession "signature"
         createdTime <- liftIO getCurrentTime

         runDB $ do
            insert $ Comment assignId content signature createdTime
            update assignId [DeliverableNumComments +=. 1]
         redirect $ ViewAssignmentR assignId
      _ -> do
         setMessage "Form failed"
         --redirect $ ViewAssignmentR assignId
         defaultLayout $ do [whamlet|
<form method=post>
   ^{commentWidget}
   <input type=submit>|]


postSignatureR :: Handler RepJson
postSignatureR = do
   (Signature rawsig) <- parseJsonBody_
   nid <- liftIO $ randomRIO (10000, 99999)
   let sig@(Signature s) = Signature $ (T.take 50 $ rawsig) `T.append` "-" `T.append` (T.pack $ show (nid :: Int))
   setSession "signature" s
   jsonToRepJson sig

getUnsetR :: DeliverableId -> Handler RepHtml
getUnsetR returnId = do
   deleteSession "signature"
   redirect $ ViewAssignmentR returnId





validPasswordField :: (RenderMessage master FormMessage) => Field sub master Text
validPasswordField = checkBool (== "yuplanner") ("Bad password" :: Text) passwordField

getLinkForm :: Form Text
getLinkForm = renderDivs $ areq validPasswordField "Password" Nothing


getRedirR :: Handler RepHtml
getRedirR = do
   redirect DateR

------------------------
--                    --
-- Utility Functions  --
--                    --
------------------------

data DAssignment = DAssignment
   { section :: Text
   , assignmentName :: Text
   , description :: Textarea
   , password :: Text
   }
   deriving Show

assignmentForm :: Html -> MForm App App (FormResult DAssignment, Widget)
assignmentForm = renderDivs $ DAssignment
   <$> areq (selectFieldList sections) "Section" Nothing
   <*> areq textField "Assignment Name" Nothing
   <*> areq textareaField "Description" Nothing
   <*> areq passwordField "Entry Code" Nothing

commentForm :: Form (Html, Text)
commentForm = renderDivs $ (,)
   <$> areq nicHtmlField "Comment" Nothing
   <*> areq validPasswordField "Entry Code" Nothing

   
sections :: [(Text,Text)]
sections = [("Business Communications", "BC"),
            ("Professional Skills", "Pro"),
            ("Internship Essentials", "I.S.E"),
            ("Technology & Troubleshooting Essentials", "T.T.E"),
            ("Independent Learning Lab", "I.L.L"),
            ("Other", "O")]

valToKey :: Text -> [(Text,Text)] -> Text
valToKey text [] = "No Class"
valToKey text ((a,b):xs) = if text == b then a
                                        else valToKey text xs
sectionToClass :: Text -> Text
sectionToClass section = case section of
   "I.S.E" -> "ISE"
   "T.T.E" -> "TTE"
   "O" -> "Other"
   "I.L.L" -> "ILL"
   _ -> section

showT :: (Show a) => a -> Text
showT showable = T.pack $ show showable

readT :: (Read a) => Text -> a
readT text = read $ T.unpack text

validateAssignment :: Day -> DAssignment -> Maybe Deliverable
validateAssignment day (DAssignment s a d _) = if weekDay day
   then Just $ Deliverable s a (unTextarea d) day False 0
   else Nothing

entryCode :: Text
entryCode = "yuplanner"

addNothings :: Day -> [[Entity Deliverable]] -> [Maybe [(Entity Deliverable)]]
addNothings day xxs = foldl (\acc xs -> safeReplaceElement acc (getElementIndex xs) (Just xs)) nothings xxs
   where originalOffset = if gpm day > 4 then 0 else gpm day
         offset entityList = gpm $ dayFromEntityList entityList
         dayFromEntityList ((Entity _ (Deliverable _ _ _ due _ _)):_) = due 
         getElementIndex entityList = let naieve = offset entityList - originalOffset
                                      in if naieve < 0 then 5 + naieve
                                         else naieve
         nothings = map (const Nothing) [1..5]

getDueThatDay :: Day -> [[Entity Deliverable]] -> Maybe [Entity Deliverable]
getDueThatDay day xxs = findList xxs
   where dayFromEntityList ((Entity _ (Deliverable _ _ _ due _ _)):_) = due
         findList [] = Nothing
         findList (x:xs) = if dayFromEntityList x == day then Just x
                                                         else findList xs

splitDels :: [Entity Deliverable] -> [[Entity Deliverable]]
splitDels dels = groupBy delTest dels
   where delTest (Entity _ (Deliverable _ _ _ due1 _ _))
                 (Entity _ (Deliverable _ _ _ due2 _ _)) = due1 == due2

safeReplaceElement :: [a] -> Int -> a -> [a] 
safeReplaceElement xs i x = 
   if i >= 0 && i < length xs 
      then replaceElement xs i x 
      else xs 
replaceElement :: [a] -> Int -> a -> [a] 
replaceElement xs i x = fore ++ (x : aft) 
   where fore = take i xs 
         aft = drop (i+1) xs

getLeft :: Bool -> Int -> (Text, Int)
getLeft _ 0 = ("n", 1)
getLeft False offset = ("p", offset - 1)
getLeft _ offset = ("n", offset + 1)

getRight :: Bool -> Int -> (Text, Int)
getRight True 1 = ("p", 0)
getRight True offset = ("n", offset - 1)
getRight _ offset = ("p", offset + 1)

