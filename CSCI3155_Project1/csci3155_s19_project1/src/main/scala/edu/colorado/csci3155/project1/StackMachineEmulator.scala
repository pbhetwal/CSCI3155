//Name: Parikshit Bhetwal
package edu.colorado.csci3155.project1


sealed trait StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack and a single instruction of type StackMachineInstruction
        Return a stack that results when the instruction is executed from the stack.
        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.

     */
    def emulateSingleInstruction(stack: List[Double], ins: StackMachineInstruction): List[Double] = {
        ins match{
          case AddI => stack match{
            case Nil => throw new IllegalArgumentException()
            case i1 :: i2 :: tail => i2 :: tail match{
              case Nil => throw new IllegalArgumentException()
              case i2 :: tail => i1 + i2 :: tail
            }
          }
          //USE V1 and V2
          case SubI => stack match{
            case Nil => throw new IllegalArgumentException()
            case i1 :: i2 :: tail => i2 :: tail match{
              case Nil => throw new IllegalArgumentException()
              case i2 :: tail => {
                val v1 = i1 
                val v2 = i2
                (i2 - i1) :: tail
              }
            }
          }
          case MultI => stack match{
            case Nil => throw new IllegalArgumentException()
            case i1 :: i2 :: tail => i2 :: tail match{
              case Nil => throw new IllegalArgumentException()
              case i2 :: tail => i1 * i2 :: tail
            }
          }
          //USE V1 and V2 
          case DivI => stack match{
            case Nil => throw new IllegalArgumentException()
            case i1 :: i2 :: tail => i2 :: tail match{
              case Nil => throw new IllegalArgumentException()
              case i2 :: tail =>{
                val v1 = i1 
                val v2 = i2 
                (v2 / v1) :: tail
              }
            }
          }
          case ExpI => stack match{
            case Nil => throw new IllegalArgumentException()
            case head :: tail => {
              scala.math.exp(head) :: tail 
            }
          }
          case LogI => stack match{
            case Nil => throw new IllegalArgumentException()
            case head :: tail => { 
              if (head > 0){scala.math.log(head) :: tail}
              else{throw new IllegalArgumentException()}
            }
          }
          case SinI => stack match{
            case Nil => throw new IllegalArgumentException()
            case head :: tail => {
              scala.math.sin(head) :: tail 
            }
          }
          case CosI => stack match{
            case Nil => throw new IllegalArgumentException()
            case head :: tail => {
              scala.math.cos(head) :: tail 
            }
          }
          case PushI(f) => f :: stack 

          case PopI => stack match{
            case Nil => throw new IllegalArgumentException()
            case i1 :: tail => {
              tail 
            }
          }
        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be a double that is the top of the stack after all instructions
       are executed.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Double =
        instructionList.foldLeft(Nil : List[Double])((acc:List[Double], SMI:StackMachineInstruction) => {
          emulateSingleInstruction(acc, SMI)
          })(0)

}