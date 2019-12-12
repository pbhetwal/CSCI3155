//Name: Parikshit Bhetwal
package edu.colorado.csci3155.project1

object Main {
    def main(args: Array[String]) = {
        val e = Plus(Log(Plus(Const(1.0), Mult(Const(3.0), Exp(Const(4.1))))), Const(1.0))
        val instrList:List[StackMachineInstruction] = StackMachineCompiler.compileToStackMachineCode(e)
        println("Compiled Instructions")
        instrList.foreach {
            case ins => println(ins)
        }
        println("Emulated Value")
        println(StackMachineEmulator.emulateStackMachine(instrList))

        val e1 = Minus(Const(2.0), Minus(Const(3.0), Const(5.0)))
        val instrList2:List[StackMachineInstruction] = StackMachineCompiler.compileToStackMachineCode(e1)
        println("Compiled Instructions")
        instrList2.foreach {
            case ins => println(ins)
        }
        println("---------------- TEST 1 ----------------")
        println("Solution to 1 + (2 - 3) * 5 = -4")
        val insts1 = List(PushI(1.0),PushI(2.0),PushI(3.0),SubI,PushI(5.0),MultI,AddI)
        println("Emulated from instructions: " + StackMachineEmulator.emulateStackMachine(insts1))
        val AST1 = Plus(Const(1.0), Mult(Minus(Const(2.0), Const(3.0)), Const(5.0)))
        val AST1insts:List[StackMachineInstruction] = StackMachineCompiler.compileToStackMachineCode(AST1)
        println("Emulated from AST: " + StackMachineEmulator.emulateStackMachine(AST1insts) + "\n")


        println("---------------- TEST 2 ----------------")
        val AST2 = Div(Plus(Const(10.0), Const(6.0)), Const(8.0))
        val AST2insts: List[StackMachineInstruction] = StackMachineCompiler.compileToStackMachineCode(AST2)
        println("Compiled Instructions")
        AST2insts.foreach {
            case ins => println(ins)
        }
        println("Solution to (10 + 6) / 8 = 2")
        println("Test 2 Emulated Value: " + StackMachineEmulator.emulateStackMachine(AST2insts) + "\n")


        println("---------------- TEST 3 ----------------")
        // NOTE: Sin in Scala.math.sin takes in an argument in radians!!!!
        val AST3 = Div(Plus(Exp(Const(2.0)), Mult(Plus(Const(4.0), Exp(Const(3.0))), Log(Const(7.0)))), Sine(Const(6.0)))
        val AST3insts: List[StackMachineInstruction] = StackMachineCompiler.compileToStackMachineCode(AST3)
        println("Compiled Instructions")
        AST3insts.foreach {
            case ins => println(ins)
        }
        println("Solution to (e^2 + ((4 + e^3) * log(7))) / sin(6) = -194.18159405...")
        println("Test 3 Emulated Value: " + StackMachineEmulator.emulateStackMachine(AST3insts) + "\n")


        println("---------------- TEST 4 ----------------")
        println("Testing if empty list List() throws illegal argument exception\n")
        val lst1 = List()
        var passed = false
        try{
            println("Instructions:\n")
            println("N/A")
            println("\nSHOULD THROW EXCEPTION")
            StackMachineEmulator.emulateStackMachine(lst1)
        }
        catch{
            case illegalArgumentException:Throwable => {
                passed = true 
                println(s"\n*** TEST 4 PASSED ***\n")
            }
        }
        assert(passed == true, "TEST 4 Failed")


        println("---------------- TEST 5 ----------------")
        println("Testing if illegal operation List(PushI(2.5), AddI) throws illegal argument exception\n")
        val lst2 = List(PushI(2.5), AddI)
        var passed2 = false
        try{
            println("Instructions:\n")
            lst2.foreach(println)
            println("\nSHOULD THROW EXCEPTION")
            StackMachineEmulator.emulateStackMachine(lst2)
        }
        catch{
            case illegalArgumentException:Throwable => {
                passed2 = true 
                println(s"\n*** TEST 5 PASSED ***\n")
            }
        }
        assert(passed2 == true, "TEST 5 Failed")
        

        
    }
}
