-- ▪Dado o tipo Hora = (Int,Int,Int), composto por horas,
-- minutos e segundos, crie as seguintes funções:
    -- ▪Recebe uma Hora e informa se é válida

    -- ▪Recebe uma Hora e retorna a quantidade de
    -- segundos desde a hora zero

    -- ▪Recebe um valor inteiro representando segundos e
    -- converte na hora correspondente

    -- ▪Recebe dois argumentos do tipo hora e retorna a
    -- diferença de tempo entre as duas


type Hora = (Int, Int, Int)

horaValida :: Hora -> Bool
horaValida (h, m, s) = h >= 0 && h < 24 &&
                       m >= 0 && m < 60 &&
                       s >= 0 && s < 60

horaParaSegundos :: Hora -> Int
horaParaSegundos (h, m, s) = h * 3600 + m * 60 + s

segundosParaHora :: Int -> Hora
segundosParaHora totalSegundos =
  let h = totalSegundos `div` 3600
      restoHoras = totalSegundos `mod` 3600
      m = restoHoras `div` 60
      s = restoHoras `mod` 60
  in (h, m, s)

diferencaHoras :: Hora -> Hora -> Hora
diferencaHoras hora1 hora2 = segundosParaHora (abs (horaParaSegundos hora1 - horaParaSegundos hora2))

teste::Hora -> String
teste = show

soma:: Int -> Int -> Int
soma a n = a + n

