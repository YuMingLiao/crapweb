module Main where
import Prelude hiding (lookup)
import qualified Prelude as P (lookup)
import qualified Prelude as P (lookup)
import Servant ( (:>), (:<|>) )
import qualified Servant as S
import qualified Network.Wai.Handler.Warp as W
import Servant.HTML.Blaze as SB
import qualified Text.Blaze.Html5 as B
import Registration
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap as IntMap
import qualified Text.Digestive as DF
import Text.Digestive ((.:))
import Data.Monoid ( (<>) )
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as BA
import Text.Digestive.Blaze.Html5 as DB
import qualified Data.Maybe as M
import qualified Data.Text as T
import Control.Lens
import Data.Time
import Text.Email.Validate (validate, isValid)
import Data.Text.Encoding (encodeUtf8)

type PingAPI = "ping" :> S.Get '[S.PlainText] String

type HtmlPingAPI = "htmlping" :> S.Get '[SB.HTML] B.Html

type UserAPI = "user" :> S.Capture "id" Int :> S.Get '[SB.HTML] B.Html

type UserPostAPI = "user" :> S.Capture "id" Int :> S.ReqBody '[S.FormUrlEncoded] [(String,String)] :> S.Post '[HTML] B.Html


type API = PingAPI
      :<|> HtmlPingAPI
      :<|> UserAPI
      :<|> UserPostAPI

api :: S.Proxy API
api = S.Proxy

app = S.serve api server

server :: S.Server API
server = handlePing S.:<|> 
         handleHtmlPing S.:<|>
         handleUser S.:<|> 
         handleUserPost

main :: IO ()
main = W.run 8080 app

handlePing :: S.Handler String
handlePing = return "PONG"

-- handleHtmlPing :: S.Handler 
-- handleHtmlPing = return "Some basic HTML"
handleHtmlPing :: S.Handler B.Html
handleHtmlPing = return $ B.docTypeHtml $ do
  B.head $ do
    B.title "HTMLPONG"
  B.body $ do
    B.h1 "HTML Ping Response"
    B.p "It seems to work ok"

handleUser :: Int -> S.Handler B.Html
handleUser identifier = do
  res <- liftIO $ lookupUser identifier
  case res of
       Nothing -> handleLookupErr identifier
       Just user -> return $ B.docTypeHtml $ do
            B.body $ do
              B.h1 $ do "User "
                        B.toHtml (show identifier)
              view <- DF.getForm "User" (userDigestiveForm user)
              htmlForUser view

htmlForUser :: DF.View B.Html -> B.Html
htmlForUser view =
  B.form
    ! BA.method "post"
    $ do
      B.p $ do  "name: "
                DB.errorList "userName" view
                DB.inputText "userName" view
      B.p $ do  "email: "
                DB.errorList "userMail" view
                DB.inputText "userMail" view
      B.p $ do  "registration time: "
                DB.inputText "registrationTime" view
      B.p $     DB.inputSubmit "Save" 

handleLookupErr :: Int -> S.Handler B.Html
handleLookupErr identifier = return $ B.docTypeHtml $ do
  B.body $ B.p $ do "Can't find user with ID "
                    B.toHtml (show identifier)

handleUserPost :: Int -> [(String, String)] -> S.Handler B.Html
handleUserPost identifier reqBody = do
  res <- liftIO $ lookupUser identifier
  liftIO $ print reqBody
  case res of
       Nothing -> handleLookupErr identifier
       Just user -> do
         viewValue <- DF.postForm "User" (userDigestiveForm user) (servantPathEnv reqBody)
         case viewValue of
            (view, Nothing) -> 
              return $ B.docTypeHtml $ do
                B.body $ do
                  B.h1 $ do "User (there were errors): "
                            B.toHtml (show identifier)
                  htmlForUser view
            (_, Just newUser) -> do
              now <- liftIO getCurrentTime
              liftIO $ addUser' (set registrationTime (Just now) newUser)
              return "Record updated."

userDigestiveForm :: Monad m => User -> DF.Form B.Html m User 
userDigestiveForm initial = do
 User 
    <$> "userName" .: nonEmptyText (Just $ initial^.userName)
    <*> "userMail" .: validEmail (Just $ initial^.userMail)
    <*> "registrationTime" .: DF.stringRead "error" (Just $ initial^.registrationTime)

servantPathEnv :: Monad m => [(String, String)] -> DF.FormEncType -> m (DF.Env m)
servantPathEnv reqBody _ = return env
  where
      pathAsString = T.unpack . DF.fromPath
      packAsInput = DF.TextInput . T.pack
      lookupParam p = P.lookup (pathAsString p) reqBody
      env path = return (packAsInput <$> (M.maybeToList (lookupParam path)))


--validation
nonEmptyText def =
    (DF.check "This field must not be empty" (/= ""))
  $ DF.text def

validEmail def =
    (DF.check "This field must be a valid email." (isValid . encodeUtf8))
  $ nonEmptyText def
