import Control.Monad (liftM, MonadPlus(..), guard)
import Control.Monad.Trans (MonadTrans(..))

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
    return = MaybeT. return . Just
    x >>= f = MaybeT $ do
        mval <- runMaybeT x
        case mval of
            Nothing -> return Nothing
            Just val -> runMaybeT $ f val

instance Monad m => MonadPlus (MaybeT m) where
    mzero = MaybeT $ return Nothing
    mplus x y = MaybeT $ do
        mval <- runMaybeT x
        case mval of
            Nothing -> runMaybeT y
            Just _ -> runMaybeT x

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

getValidPassword :: MaybeT IO String
getValidPassword = do
    s <- lift getLine
    guard (isValid s)
    return s

askPassword :: MaybeT IO ()
askPassword = do
    lift $ putStrLn "Insert your new password: "
    _ <- getValidPassword
    lift $ putStrLn "Storing in database ..."

isValid :: String -> Bool
isValid s = length s >= 8

main :: IO ()
main = do
    a <- runMaybeT askPassword
    print a
