/**
 * Bank class realization using semaphores.
 * Same logic as synchronized, just explicitly provide exclusion via locks (using a semaphore)
 */

package bank;

import handler.*;
import java.util.concurrent.Semaphore;

public class Semaph extends Abstraction {
    Semaphore synchS; // synchronization semaphore
    Semaphore moneyS; // used to track money >=0
    // well, this is not a "classical" semaphore, but assignment said "using semaphores in Java"
    // so I assume we are free to use java-semaphore that can acquire/release multiple permits at once..
    // Otherwise, using classic/binary semaphores only, we are pretty much limited to
    // an explicit busy-wait loop..

    public Semaph(Log logger) {
        super(logger);
        this.synchS = new Semaphore(1); // pure exclusion lock, binary
        this.moneyS = new Semaphore(0); // money starts at 0, upcounting
    }

    public Semaph(Log logger, int initial) throws NegativeMoney {
        super(logger,initial);
        this.synchS = new Semaphore(1);
        this.moneyS = new Semaphore(initial);
    }


    @Override
    public void put (int amount) throws InterruptedException {
        // this one requires only serialized access, no checks
        try {
            this.synchS.acquire();
            this.money += amount;
            logger.logBankAction(Log.MoneyAction.INCREASE, this.money);
            this.moneyS.release(amount);
            // see comment in get method about the use of moneyS tracker..
        } finally {
            this.synchS.release();
        }
    }

    // Taking resources is more tricky as we have a condition (money >=0)
    // Java does not have guarded task entries (as Ada does), so we have to implement this ourselves.
    @Override
    public  void get (int amount) throws InterruptedException {
        // Classical (binary) semaphore solution uses busy-wait loop here.
        // But since we are not apparently prohibited to use java-extanded semaphores,
        // we lock on money-tracking semaphore
        //
        // NOTE: (!!!)
        // GUI version can deadlock upon termination (pressing stop button) in the case
        // when too many children are waiting for too much money:
        // when parents are stopped after their last cycle, there may not be enough money
        // to release all the children. And so we are left waiting forever on that Stop button to turn "green"..
        //
        // To fix this we can a) either make parents dump money until there is demand - too complex logic
        // or b) make children succeed with their "get" unconditionally in the last cycle.
        // b is a much more robust solution in our case (as no specific limitations are set
        // on money flow during termination - in fact no termination conditions are specified at all!)
        // Moreover. it is much easier (and less error-prone) to implement this logic here,
        // together with all synchronization..
        if (Params.isRunning) {
            this.moneyS.acquire(amount);
            // past this we are guaranteed to have enough money, even if multiple threads
            // pass this at once, total permits are still positive.
            // The fiirst one to overflow will be blocked..
            //
            // Need to synchronize money access still
            // *after*, not before above lock, or we risk deadlocking..
            this.synchS.acquire();
            this.money -= amount;
            logger.logBankAction(Log.MoneyAction.DECREASE, this.money);
            this.synchS.release();
        }
    }

    @Override
    public void set (int amount)// throws InterruptedException
    {
        // need to override this one here, to set money-tracking semaphore correctly
        super.set(amount);
        this.moneyS.drainPermits();
        this.moneyS.release(amount);
    }

}
