/**
 * top level abstract bank class, defining interface and common data
 */

package bank;
import handler.Log;

public abstract class Abstraction {

    class NegativeMoney extends Exception {}

    volatile int money;
    Log logger;

    public Abstraction(Log logger) {
        this.logger =logger;
        this.money  = 0;
    }
    public Abstraction(Log logger, int initial){
        this.logger = logger;
        if (initial < 0) this.money = 0; // normally we would "throw new NegativeMoney();"", but lets keep things simple here
        else this.money  = initial;
    }

    public abstract void get (int amount) throws InterruptedException;
    public abstract void put (int amount) throws InterruptedException;

    // We need this one for GUI version - to let user change total between runs
    //
    // NOTE: this directly sets money to given amount, bypassing locks and checks
    // (otherwise we deadlock during re-run setup).
    // This one is not to be called directly by Parent/Child classes!!
    // Its intended to be only called from a single (setup) thread.
    public void set (int amount) {
        if (amount < 0) this.money = 0;
        else this.money = amount;
    }
}

