import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Text.Printf

data Bebida = PepiseCola | GuaranáPoloNorte | GuaranáQuate
  deriving (Show, Eq)

type Maquina = MVar (Int, Int, Int)

main :: IO ()
main = do
  maquina <- newMVar (2000, 2000, 2000)
  let clientes = [(1, PepiseCola), (2, PepiseCola), (3, GuaranáPoloNorte), (4, GuaranáQuate)]
  mapM_ (forkIO . cliente maquina) clientes
  reporBebidas maquina

cliente :: Maquina -> (Int, Bebida) -> IO ()
cliente maquina (n, bebida) = forever $ do
  threadDelay 1000000
  modifyMVar_ maquina $ \est -> do
    let (q1, q2, q3) = est
    case bebida of
      PepiseCola -> if q1 >= 300 then do
                      printf "O cliente %d do refrigerante %s está enchendo seu copo\n" n (show bebida)
                      return (q1 - 300, q2, q3)
                    else return est
      GuaranáPoloNorte -> if q2 >= 300 then do
                            printf "O cliente %d do refrigerante %s está enchendo seu copo\n" n (show bebida)
                            return (q1, q2 - 300, q3)
                          else return est
      GuaranáQuate -> if q3 >= 300 then do
                        printf "O cliente %d do refrigerante %s está enchendo seu copo\n" n (show bebida)
                        return (q1, q2, q3 - 300)
                      else return est

reporBebidas :: Maquina -> IO ()
reporBebidas maquina = forever $ do
  threadDelay 1500000
  modifyMVar_ maquina $ \est -> do
    let (q1, q2, q3) = est
    let f q bebida = if q < 1000 then do
                      putStrLn $ "O refrigerante " ++ show bebida ++ " foi reabastecido com 1000 ml, e agora possui " ++ show (q + 1000) ++ " ml"
                      return (q + 1000)
                    else return q
    q1' <- f q1 PepiseCola
    q2' <- f q2 GuaranáPoloNorte
    q3' <- f q3 GuaranáQuate
    return (q1', q2', q3')
