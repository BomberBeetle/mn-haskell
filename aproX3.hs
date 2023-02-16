calcRiemann x bound step = if x>=bound then 0 else (x*x) + calcRiemann (x+step) bound step

calcIntegration = (4*4*4)/3

calcError = calcIntegration - calcRiemann 0 4 1

main = do
    print ("Calculo via Riemann: " ++ show (calcRiemann 0 4 1))
    print ("Calculo via integracao: " ++ show calcIntegration)
    print ("Erro: " ++ show calcError)
