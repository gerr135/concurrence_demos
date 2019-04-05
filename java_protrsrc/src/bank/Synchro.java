/**
 * top level abstract bank class, defining interface and common data
 */

package bank;

import handler.*;

public class Synchro extends Abstraction {
    public Synchro(Log logger) { super(logger); }
    public Synchro(Log logger, int initial) throws NegativeMoney { super(logger,initial); }

    @Override
    public synchronized void put (int amount) {
        // this one requires only serialized access, no checks
        this.money += amount;
        this.notifyAll();
        logger.logBankAction(Log.MoneyAction.INCREASE, this.money);
    }

    // Taking resources is more tricky as we have a condition (money >=0)
    // Java does not have guarded task entries (as Ada does), so we have to implement this ourselves.
    @Override
    public synchronized void get (int amount) throws InterruptedException {
        // This should make the calling thread wait until enough money is present.
        // Uses monitors internally to sleep instead of busy-waiting.
        while (amount > this.money) {
            // wait until deposit and recheck
            this.wait();
        }
        this.money -= amount;
        logger.logBankAction(Log.MoneyAction.DECREASE, amount);
    }

    // An extra.
    // This Non-blocking checkAndGet mimics the "family routine"
    // each child runs up to check if there is enough money, and then takes or runs away,
    // to return at a later point. This allows clients to keep doing useful stuff if they want..
    // NOTE1: this obviously requires changing the client behavior.
    // NOTE2: potentially we can starve a child, but we are not asked to prevent starvation here.
    // In fact, clients can just check at regular intervals (the same for every child),
    // which will prevent starvation in all but few very special cases..
    public synchronized boolean checkAndGet(int amount){
        // returns immediately.
        // check if enough money is present, allocate resources and return true if yes,
        // return false if no
        if (amount <= this.money) {
            this.money -= amount;
            logger.logBankAction(Log.MoneyAction.DECREASE, this.money);
            return true;
        } else {return false;}
    }
}
