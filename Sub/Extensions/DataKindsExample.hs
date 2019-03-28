{-# LANGUAGE DataKinds #-}

module Sub.DataKindsExample where


data ServiceType = Telegram | Slack

instance Show 'Telegram where 
    show tg = "tg"

foo :: 'Telegram -> String
foo a = "qwe"