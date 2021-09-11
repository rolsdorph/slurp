module RabbitMQ
  ( QueueConsumer,
    ConsumerRegistry,
    createConsumerRegistry
  )
where

import Control.Monad (void)
import qualified Network.AMQP as Q
import Types (QueueConfig (..))

type QueueConsumer = Q.Message -> IO ()

type ConsumerRegistry = QueueConsumer -> IO ()

createConsumerRegistry :: QueueConfig -> IO ConsumerRegistry
createConsumerRegistry queueConfig = do
  conn <-
    Q.openConnection
      (hostname queueConfig)
      (vhost queueConfig)
      (username queueConfig)
      (password queueConfig)
  chan <- Q.openChannel conn
  _ <- Q.declareQueue chan $ Q.newQueue {Q.queueName = notiQueueName queueConfig}

  return $ \h -> void (Q.consumeMsgs chan (notiQueueName queueConfig) Q.NoAck (\(m, _) -> h m))
