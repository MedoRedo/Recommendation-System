import System.Random
import System.IO.Unsafe
users = [ "user1" , "user2" , "user3" , "user4" ]
items = [ "item1" , "item2" , "item3" , "item4" , "item5" , "item6" ]
purchasesHistory = [("user1" , [ [ "item1" , "item2" , "item3" ] , [ "item1" , "item2" , "item4" ] ] ) ,("user2" , [ [ "item2" , "item5" ] , [ "item4" , "item5" ] ]),("user3" , [ [ "item3" , "item2" ] ] ) ,("user4" , [ ] )]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats [] = []
getAllUsersStats ((user,pre):ys) = (user,(getUserStats user items pre)) : (getAllUsersStats ys)  

allUsersStats = getAllUsersStats purchasesHistory
d xs = filter (/= ' ') xs 
purchasesIntersection xs []=[]
purchasesIntersection xs ((u,l):us) =  (helperInter xs l):(purchasesIntersection xs us)
helperInter [] [] = []
helperInter ((_,x):xs) ((e,y):ys) = if(x/=[] && y/=[]) then (e,addfreq x y):helperInter xs ys else helperInter xs ys   

freqListUsers:: String  -> [(String, Int)] 
freqListUsers user = addfreq2 h (split_ m)     where (l ,m ,h) = (helpero user allUsersStats, purchasesIntersection l (filter (/=(user,l)) allUsersStats),addfreqh(split_ m))


addfreqh :: [(String,Int)] -> [String]
addfreqh [] = [] 
addfreqh ((i,f):xs) =  if(elem i r) then addfreqh xs else i:addfreqh xs    where r = addfreqh xs


addfreq2 [] _ = []
addfreq2 (x:xs) ys  = (x,calculateF x ys):addfreq2 xs ys 


calculateF :: (Eq a) => a -> [(a,Int)] -> Int
calculateF x [] = 0
calculateF x ((e,f):xs) = if(x==e) then f+calculateF x xs else calculateF x xs								 

split_ [] = []
split_ (x:xs) = (split2 x) ++ (split_ xs)
split2 [] = []
split2 ((e,ef):xs) = ef ++ split2 xs   

helpero user []=[]
helpero user ((u,x):xs)=if(user==u) then x else (helpero user xs)
 
addfreq x y=removed (addfreqq x y)

removed []=[]
removed (x:xs) = if(elem x y) then removed xs else x:removed xs where y=removed xs

addfreqq x y = (addfreqq2 x y) ++ (addfreqq2 y x)
 
addfreqq2 [] _ = []
addfreqq2 ((x,l1):xs) y = (x,(l1+finddd x y)):(addfreqq2 xs y)

finddd x []=0
finddd x ((y,n):ys) = if(x==y) then n
				  else finddd x ys



recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user = if(length space /= 0) then space !! randomZeroToX (length space - 1 ) else []  where space = expand (freqListUsers user)

recommend :: String -> [String] -> String
recommend user cart  = if(o1/="" && o2/="") then [o1,o2] !! randomZeroToX 1 else if(o1/="")then o1 else if(o2/="")then o2 else items !! randomZeroToX (length items - 1 )                      
						where(o1,o2) = (recommendBasedOnUsers user,recommendBasedOnItemsInCart user cart)


freqListItems:: String -> [(String, Int)]

freqListItems user = sumfreq (getUserStats user items (findUser user purchasesHistory))  

freqListCart:: String ->[String] -> [(String, Int)]
freqListCart user cart = freqListCartHelper items cart (findUser user purchasesHistory)

freqListCartHelper [] ys pre = []
freqListCartHelper (x:xs) ys pre = if(z>0) then(x,z): (freqListCartHelper xs ys pre) else (freqListCartHelper xs ys pre) where z=(freqListCartHelper2 x ys pre) 

freqListCartHelper2 x [] pre = 0
freqListCartHelper2 x (y:ys)  pre =  if(x /= y && z>0) then (z+(freqListCartHelper2 x ys pre)) else (freqListCartHelper2 x ys pre)  where z=freq x y pre 

freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user cart =  addfreq (freqListCart user cart) (freqListItems user)  

addfreqListCartAndItems [] ys = ys
addfreqListCartAndItems xs [] = xs
addfreqListCartAndItems ((item,f):xs) ((item2,f2):ys) = (item,f+f2): addfreqListCartAndItems xs ys 

sumfreq :: [(String,[(String,Int)])] -> [(String,Int)]
sumfreq [] = []
sumfreq ((i,l):xs) = if(n /= 0) then (i,n):(sumfreq xs) else sumfreq xs where n = (sumfreq2 l)
sumfreq2 [] = 0
sumfreq2 ((i,n):xs) = n+sumfreq2 xs

recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart user cart = if(length space /= 0) then space !! randomZeroToX (length space - 1 ) else []  where space = expand (freqListCartAndItems user cart) 

expand [] = []
expand ((i,f):xs) = expandh(i,f) ++ expand xs
expandh (i,0) = []
expandh (i,f) = i:expandh(i,f-1) 

getDiff [] ys = []
getDiff ((i,f):xs) ys = if(elem i ys) then getDiff xs ys else (i,f):getDiff xs ys

getMax [] = []
getMax [(i,f)] = i
getMax ((i,f):(i2,f2):xs)  = if(f>=f2) then getMax((i,f):xs) else  getMax((i2,f2):xs) 	                       

recommendEmptyCart :: String -> String
recommendEmptyCart user = recommendBasedOnItemsInCart user []  

getUserStats xs [] pre = [] 
getUserStats xs (y:ys) pre = (y,pass y items pre): getUserStats xs ys pre         

findUser user1 [] = []
findUser user1 ((user2,ys):xs) = if(user1 == user2) then ys else findUser user1 xs   

freq x y [] = 0
freq x y (z:zs) = if((elem x z) && (elem y z)) then 1+freq x y zs else freq x y zs 

pass x [] zs = []
pass x (y:ys) zs = if((x /= y) && (w>0)) then (y,w): (pass x ys zs) else  pass x ys zs              where w = freq x y zs

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList [] = []
createEmptyFreqList (x:xs) = (x,[]):createEmptyFreqList xs 