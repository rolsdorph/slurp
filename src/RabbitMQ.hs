module RabbitMQ
  ( QueueConsumer,
    ConsumerRegistry,
    createConsumerRegistry
  )
where

import Control.Monad (void)
import Data.Text
import qualified Network.AMQP as Q
import Types (QueueConfig (..))

type QueueConsumer = Q.Message -> IO ()

type ConsumerRegistry = QueueConsumer -> IO ()

createConsumerRegistry :: QueueConfig -> (QueueConfig -> Text) -> IO ConsumerRegistry
createConsumerRegistry queueConfig queue = do
  conn <-
    Q.openConnection
      (hostname queueConfig)
      (vhost queueConfig)
      (username queueConfig)
      (password queueConfig)
  chan <- Q.openChannel conn
  _ <- Q.declareQueue chan $ Q.newQueue {Q.queueName = queue queueConfig}

  return $ \h -> void (Q.consumeMsgs chan (queue queueConfig) Q.NoAck (\(m, _) -> h m))
