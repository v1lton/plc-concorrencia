import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Text.Printf

-- Bebida representa cada refrigerante
data Bebida = PepiseCola | GuaranáPoloNorte | GuaranáQuate
  deriving (Eq)

instance Show Bebida where
  show PepiseCola = "Pepsi-Cola"
  show GuaranáPoloNorte = "Guaraná Polo Norte"
  show GuaranáQuate = "Guaraná Quate"

-- Máquina de refrigerante
type Estoque = (Int, Int, Int)

type Maquina = MVar Estoque

main :: IO ()
main = do
  -- Máquina com 2000ml de cada refrigerante.
  -- Considerando (PepsiCola, GuaranáPoloNorte, GuaranáQuate).
  maquina <- newMVar (2000, 2000, 2000)

  -- Construção de cada cliente. Cliente possui um ID e a bebida escolhida.
  let clientes = [(1, PepiseCola), (2, PepiseCola), (3, GuaranáPoloNorte), (4, GuaranáQuate)]

  -- Cada cliente é uma thread.
  mapM_ (forkIO . cliente maquina) clientes

  -- Reposição de bebida também possui acesso à máquina.
  reporBebidas maquina

-- Cada Cliente acessa a máquina bloqueando seu acesso.
cliente :: Maquina -> (Int, Bebida) -> IO ()
cliente maquina (n, bebida) = forever $ do
-- Cada cliente leva 1000 ms para encher seu copo com o refrigerante.
  threadDelay 1000000
  -- Bloco para modificar a máquina atomicamente.
  modifyMVar_ maquina $ \estoque -> do
    let (q1, q2, q3) = estoque
    case bebida of
      PepiseCola ->
        if q1 >= 300
          then do
            printf "O cliente %d do refrigerante %s está enchendo seu copo\n" n (show bebida)
            return (q1 - 300, q2, q3)
          else return estoque
      GuaranáPoloNorte ->
        if q2 >= 300
          then do
            printf "O cliente %d do refrigerante %s está enchendo seu copo\n" n (show bebida)
            return (q1, q2 - 300, q3)
          else return estoque
      GuaranáQuate ->
        if q3 >= 300
          then do
            printf "O cliente %d do refrigerante %s está enchendo seu copo\n" n (show bebida)
            return (q1, q2, q3 - 300)
          else return estoque

-- Reposição de bebida
reporBebidas :: Maquina -> IO ()
reporBebidas maquina = forever $ do
-- A máquina demora 1500 ms para reposição.
  threadDelay 1500000
  -- Bloco para modificar a máquina atomicamente.
  modifyMVar_ maquina $ \estoque -> do
    let (q1, q2, q3) = estoque
    let f q bebida =
          if q < 1000
            then do
              putStrLn $ "O refrigerante " ++ show bebida ++ " foi reabastecido com 1000 ml, e agora possui " ++ show (q + 1000) ++ " ml"
              return (q + 1000)
            else return q
    q1' <- f q1 PepiseCola
    q2' <- f q2 GuaranáPoloNorte
    q3' <- f q3 GuaranáQuate
    return (q1', q2', q3')
