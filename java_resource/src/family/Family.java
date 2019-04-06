/*
 * Encapsulating class - to keep Parent and Child threads together
 * as (in the GUI version) we need to stop and restart them..
 *
 * All methods take values directly from Params.
 * The running threads listen on Params.isRunning and will stop after competing next iteration,
 * so we only provide join method and not stop.
 */

package family;

import java.util.*;
import handler.*;

public class Family {
    ArrayList<Thread> parents;
    ArrayList<Thread> children;

    public Family(){
        // create parents list, children list to be created as needed, as number may change each run..
        this.parents  = new ArrayList<>();
        this.children = new ArrayList<>();
    }


    public void start(){
        // create and save threads
        // (we could create anonymous threads here, there is no real need
        // for an explicit stop/join call in our case, as all threads listen on a global flag.
        // However this might cause complaints about "real world scenario",
        // so better provide this functionality..
        for (int i=0; i<Params.NParents; i++)
            this.parents.add(new Thread(new Parent(i+1, Globals.logger, Globals.theBank)));
        for (int i=0; i<Params.NChildren; i++)
            this.children.add(new Thread(new Child(i+1, Globals.logger, Globals.theBank)));
        // start all threads
        this.parents.forEach (p -> p.start());
        this.children.forEach(c -> c.start());
    }

    public void join(){
        // wait for all threads
        try {
            // java doesn't understand its own syntax:
            // if we use forEach(p -> p.join()) here, it first
            // complains about having to catch InterruptedException
            // and then right after (where the catch is now) it says that it is not thrown!
            // so we have to use a regular for loop here instead
            for (Thread p : this.parents)  {p.join(Params.IncomeDelayMean + Params.IncomeDelayDlta);}
            for (Thread c : this.children) {c.join(Params.SpendDelayMean  + Params.SpendDelayDlta);}
            // these delays should be sufficient
            // NOTE: timeout on joins is necessary as sometimesjava runtime gets overwhelmed by all starts and stops,
            // especially if the Start/Stop button is clicked too often or too much..
        } catch (InterruptedException e){
        } finally {
            // all threads will get destroyed soon after, so lets ensure we are explicit about our lists being empty
            this.parents.clear();
            this.children.clear();
        }
    }
}
