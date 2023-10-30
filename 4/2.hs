zadanie2 =	[(x,y,z) | x<-[1..20],y<-[1..20],z<-[1..20],x<y,y<z, x^2+y^2==z^2]
prosto 1 = True
prosto x = and [ x `mod` y /= 0 | y <- [2..(x-1)] ]
nod x 0 = x
nod x y = nod (y)( rem x y)

zadanie3 =	[(x,y,z) | x<-[1..100],y<-[1..100],z<-[1..100], x<y, y<z,nod x y == 1, nod y z ==1, nod x z ==1 , x^2+y^2==z^2]

-- [(x,y,z) | x<-[1..20],y<-[1..20],z<-[1..20],f<-[2..(x-1)],l<-[2..(y-1)],k<-[2..(z-1)],x<y,y<z,mod x f /= 0 || mod y l /=0 ||  mod z k /=0 && x^2+y^2==z^2]
-- 2^(p-1)*(2^p-1)

--zadan4 = [ x |x<-[9999],  p<-[2..15], y<-[2..2^p-2],mod (2^p-1) (y) /= 0] 
zadanie4 = [ 2^(p-1)*(2^p-1) | p<-[2..15], d<-[1..((2^p)-2)],prosto (2^p-1)] -- if в prosto не получается


--rast (x:xs) = (

--vuch x = z1 x
perevod::[(a,a)]->[a]
perevod [(a,b)] = [a,b]
z1 [] = []
z1 (x:y:xs) = (perevod x):(perevod y) : z1 (xs)
x1 (x:xs) = x


a (x:y:s) = x+1:y*6:s


