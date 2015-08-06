{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import Image
import Stack
import Run

import           Control.Applicative
import           Control.Monad.Morph
import           Control.Monad.Trans.AWS      (AWST)
import           Data.ByteString.Builder      (Builder)
import           Data.Monoid
import           Data.Text                    (Text )
import qualified Data.Text                    as T (pack) 
import qualified Network.AWS.CloudFormation   as CF
import qualified Options.Applicative          as OP

default (Builder)

stackName :: Text
stackName = "demo-stack"

stackTemplateFile :: FilePath
stackTemplateFile = "stack-template.json"

stackTemplateObjectKey :: Text
stackTemplateObjectKey = "demo-stack-template"

keyPairName :: Text
keyPairName = "lazar@home"

publicKey :: FilePath
publicKey = "/home/lazar/.ssh/id_rsa.pub"

parameters :: [(Text, Text)]
parameters = [ ("KeyPairName", keyPairName)
             , ("SystemName", stackName)
             ]

createDemoStack :: AWST IO CF.CreateStackResponse
createDemoStack = do
    uploadKeyPair keyPairName publicKey
    createStack stackName stackTemplateFile parameters

deleteDemoStack :: AWST IO CF.DeleteStackResponse
deleteDemoStack = do
    deleteKeyPair keyPairName
    deleteStack stackName

data Command = CreateStack
             | DeleteStack
             | RunInstance 
               { runInstanceName :: String
               , runKeyName      :: String
               , runElasticIP    :: Maybe String
               }
             | KillInstance { killInstanceName :: String }

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

runInstanceParser :: OP.Parser Command
runInstanceParser = RunInstance
  <$> OP.argument OP.str (OP.metavar "INSTANCE-NAME")
  <*> OP.strOption (OP.short 'k' <> OP.long "key-name" <> OP.metavar "KEY-NAME")
  <*> optional (OP.strOption (OP.long "ip" <> OP.metavar "IP"))

killInstanceParser :: OP.Parser Command
killInstanceParser = KillInstance <$> OP.argument OP.str (OP.metavar "INSTANCE-NAME")

parseCommand :: OP.Parser Command
parseCommand = OP.subparser $
    OP.command "create-stack" (pure CreateStack `withInfo` "create cloud formation stack") <>
    OP.command "delete-stack" (pure DeleteStack `withInfo` "delete cloud formation stack") <>
    OP.command "run-instance" (runInstanceParser `withInfo` "launch an EC2 instance from a named image") <>
    OP.command "kill-instance" (killInstanceParser `withInfo` "save to an image and terminate an EC2 instance") 

main :: IO ()
main = OP.execParser opts >>= \case
    CreateStack -> run (hoist lift createDemoStack)
    DeleteStack -> run (hoist lift deleteDemoStack)
    RunInstance name key ip -> run (launch (T.pack name) (T.pack key) (T.pack <$> ip))
    KillInstance name -> run (terminate $ T.pack name)
  where
    opts = OP.info
        (OP.helper <*> parseCommand)
        (OP.fullDesc <> OP.header "AWS Utilities")
