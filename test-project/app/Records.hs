{-# LANGUAGE OverloadedRecordDot #-}
module Records where

data User = User 
    { first_name :: String
    , last_name :: String
    , age :: Int
    }


userFullName :: User -> String
userFullName user = user.first_name ++ " " ++ user.last_name
