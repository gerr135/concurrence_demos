/**
 * Parameters: initial values and handling
 */
package handler;

import java.util.Random;

public class Params {
    public static int BankAmount = 0;
    public static int NParents  = 2;
    public static int NChildren = 5;
    public static int IncomeMean  = 50;
    public static int IncomeDelta = 30;
    public static int SpendingMean  = 20;
    public static int SpendingDelta = 10;
    //
    public static int IncomeDelayMean = 1000; // miliseconds
    public static int IncomeDelayDlta = 200;
    public static int SpendDelayMean = 800;
    public static int SpendDelayDlta = 300;
    //
    public static int waitDelay = 100;

    public static boolean isRunning = false;

//     public Params(){
//         this.NParents  = 2;
//         this.NChildren = 5;
//         this.IncomeMean  = 50;
//         this.IncomeDelta = 30;
//         this.SpendingMean  = 20;
//         this.SpendingDelta = 10;
//         this.IncomeDelayMean = 1000; // miliseconds
//         this.IncomeDelayDlta = 200;
//         this.SpendDealyMean = 800;
//         this.SpendDelayDlta = 300;
//         this.waitDelay = 100;
//     }

    static Random rndGenerator = new Random();
    public static int rnd(int mean, int delta){
        if ((mean == 0) || (delta == 0)) return mean;
        else return mean - delta + rndGenerator.nextInt(2*delta);
    }

}
