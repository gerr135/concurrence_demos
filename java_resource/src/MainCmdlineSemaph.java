/**
 * main proc for cmdline version
 */

import handler.*;
import family.*;
import bank.Synchro;

public class MainCmdlineSemaph {


public static void main(String[] args) {
    MainCmdlineSemaph main = new MainCmdlineSemaph();
    Globals.logger  = new LogCmdline();
    Globals.theBank = new bank.Semaph(Globals.logger);
    Globals.theFamily = new family.Family();

    System.out.println("starting parallel inc/dec simulation");
    System.out.printf("running with N parents = %d, N children = %d; mean salary = %d, mean use = %d\n",
                        Params.NParents, Params.NChildren, Params.IncomeMean, Params.SpendingMean);
    System.out.print("starting parent and child threads ..");
    Params.isRunning = true;
    Globals.theBank.set(Params.BankAmount);
    Globals.theFamily.start();

    System.out.println(" done.");
}
}
