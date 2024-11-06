data DoubleMap k v = BM (Map k (Map k v)) (Map k (Map k v))
--INV. REP.:
    *Sea BM m1 m2 un double map
    *Para cada clave c1 de m1, cada clave c2 de fromJust (lookup c1 m1), sea v el valor de c2 en ese mapeo,
     sea, en caso de que c2 sea clave de m2, m3= fromJust (lookup c2 m2). Entonces v es valor de c1 en m3.
     -entonces c1 es clave en m3 y v es su valor-

assocCM :: Eq k => k -> k -> v -> DoubleMap k v -> DoubleMap k v 
assocM c1 c2 v (BM m1 m2) = BM (assocM c1 (f c2 v (lookupM c1 m1)) m1) (assocM c2 (f c1 v (lookupM c2 m2)) m2)
--costo O(log K1 + log K2): recorres 2 veces claves primarias y 2 veces claves secundarias.

f :: k -> v -> Maybe (Map k v) -> Map k v
f c v Nothing   = assocM c v emptyM
f c v (Just m)  = assocM c v m

lookupCM :: Either k k -> DoubleMap k v -> [(k, v)]
lookup (Left c)  dm = listaIzquierdaPara c dm
lookup (Right c) dm = listaDerechaPara c dm
--costo O(max (log K1 + K2 log K2) (logK2 + K1 log K1)): 
--toList O(K log K)+valorOVacio O(log K)

listaIzquierdaPara :: k -> DoubleMap k v -> [(k, v)]
listaIzquierdaPara k (BM m1 _) = toList (valorOVacio k m2)

--valorOVacio hace lookupM de k sobre m2 y, con uso de case of, devuelve emptyM si es Nothing y el map si es Just m. toList devuelve 
--lista vacía si es emptyM y la lista de elementos correspondientes si es un map con elementos.

lookupCM' :: Either k k -> DoubleMap k v -> [(k, v)]
lookupCM' e (BM m1 m2) = case (e) of
                           Left k  -> toList (valorOVacio k m1)
                           Right k -> toList (valorOVacio k m2)