object ApproxValue {
implicit def double2ApproxValue(d : Double) = new ApproxValue(d)
class ApproxValue(value : Double) {
def ~(other : Double) = {
Math.abs(value - other);
}
def ± (v:Double)={
var b = false;
if (value<v)
b = true;
b;
}
}
def main(args: Array[String]){
val a = 2.0
val b = Math.sqrt(2)
val c = b * b
println(a == c) // prints false
println(a ~ c ± 1E-12) // should print true because Math.abs(a - c) < 1E-12
}
}