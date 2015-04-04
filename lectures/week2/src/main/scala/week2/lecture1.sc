def sum(f:Int => Int)(a:Int, b:Int):Int = {

  def loop(a:Int, acc:Int):Int = {
    if (a > b) acc
    else loop(a+1, f(a) + acc)

  }

  loop(a,0)
}

val sumInts:(Int, Int) => Int = sum(x => x)

sumInts(1,5)

def Π(f:Int =>Int)(a:Int, b:Int):Int = {
  if (a>b) 1
  else f(a)*Π(f)(a+1,b)
}

Π(x=>x*x)(3,4)

def fact(b:Int) = Π(x => x)(1,b)

fact(5)

def mapReduce(f:Int=>Int, combine:(Int, Int)=>Int, identity:Int)(a:Int, b:Int):Int ={
  if (a > b) identity
  else combine(f(a), mapReduce(f,combine,identity)(a+1,b))
}

def product(f:Int=>Int)(a:Int, b:Int) = mapReduce(f, (x,y)=>x*y, 1)(a, b)

product(x=>x*x)(3,4)