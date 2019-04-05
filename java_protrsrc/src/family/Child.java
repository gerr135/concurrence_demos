/*
 * Parent class - should have the same behaviour in all cases, no need to override.
 * Splitting off to avoid code duplication.
 */

package family;

import handler.*;


public class Child  implements Runnable {
    int N; // personal number
    Log logger;
    bank.Abstraction theBank;

    public Child(int N, Log logger, bank.Abstraction theBank){
        this.N = N;
        this.logger = logger;
        this.theBank = theBank;
    }

    public void run(){
        while (Params.isRunning) {
            int money = 0;
            try {
                Thread.sleep(Params.rnd(Params.SpendDelayMean, Params.SpendDelayDlta));
                money = Params.rnd(Params.SpendingMean, Params.SpendingDelta);
                this.logger.logChildAction(Log.MoneyAction.DECREASE, this.N, money);
                theBank.get(money);
                this.logger.logChildDone(this.N);
            } catch (InterruptedException e){}
        }
    }
}
