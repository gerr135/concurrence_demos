/*
 * Parent class - should have the same behaviour in all cases, no need to override.
 * Splitting off to avoid code duplication.
 */

package family;

import handler.*;

public class Parent implements Runnable {
    int N; // personal number
    Log logger;
    bank.Abstraction theBank;

    public Parent(int N, Log logger, bank.Abstraction theBank){
        this.N = N;
//         System.out.println("in Parent constructor, got logger=" + logger + " bank=" + theBank);
        this.logger = logger;
        this.theBank = theBank;
//         System.out.println("Parent constructor finished");
    }

    public void run(){
        int money = 0;
        while (Params.isRunning) {
            try {
                Thread.sleep(Params.rnd(Params.IncomeDelayMean, Params.IncomeDelayDlta));
                money = Params.rnd(Params.IncomeMean, Params.IncomeDelta);
                this.logger.logParentAction(Log.MoneyAction.INCREASE, this.N, money);
                theBank.put(money);
                this.logger.logParentDone(this.N);
            } catch (InterruptedException e){}
        }
    }
}
