/**
 * base code for parallel sim, no gui
 */

import java.util.concurrent.atomic.AtomicInteger;
import java.util.Random;

public class FamilySim {

static void printHelp() {
    System.out.println("parallel inc/dec simulation.");
    System.out.println("usage:");
    System.out.println("    -v(v)          spit out (more) debug info");
}


static int verbose = 0;

    void doCmdLine(String[] args) {
        for (int i = 0; i< args.length; i++) {
            String opt = args[i];
            if (opt.compareTo("-v")    == 0) verbose = 1;
            else if (opt.compareTo("-vv")   == 0) verbose = 2;
        };
    }


static final int NParents  = 2;
static final int NChildren = 5;
static final int IncomeMean  = 50;
static final int IncomeDelta = 30;
static final int SpendingMean  = 20;
static final int SpendingDelta = 10;

static final int IncomeDelayMean = 1000; // miliseconds
static final int IncomeDelayDlta = 200;
static final int SpendDealyMean = 800;
static final int SpendDelayDlta = 300;

static final int waitDelay = 100;

static Random rndGenerator = new Random();

static int rnd(int mean, int delta){
    return mean - delta + rndGenerator.nextInt(2*delta);
}


    class MoneyBank implements Runnable {
        AtomicInteger money;
        MoneyBank() {this.money = new AtomicInteger(0);}

        // this one is easy, as no assiciated consition
        public synchronized void addMoney (int amount){
            this.money.getAndAdd(amount);
            System.out.printf("bank inc, money = %d;  ", this.money.get());
        }

        // this one is more tricky as we have a condition (money >=0)
        // Java does not have guarded task entries (as Ada does, but well, the same goes
        // for C and all other major languages), so we have to implement this ourselves.
        // In real production we would spend a lot of effort on queues and optimizing switching,
        // but we are nto asked to do anything so fancy here. So we just mimick the "family routine":
        // each child runs up to check if there is enough money, and then takes or runs away,
        // to return at a later point.
        // NOTE: yes, potentially we can starve a child, but we are not asked to prevent starvation here.
        // In fact, by checking at regular intervals (the same for every child) we will
        // prevent starvation in all but some very special cases..
        public synchronized boolean checkAndTakeMoney(int amount){
            // check if enough money is present, reduce and return true if yes,
            // return false and don;t touch money if no
            if (amount < this.money.get()) {
                this.money.getAndAdd(-amount);
                System.out.printf("bank dec, money = %d;  ", this.money.get());
                return true;
            } else {return false;}
        }

        public void run(){}
    }

    //  only one, shared by all, so common global is the most natural..
    MoneyBank theBank = new MoneyBank();

    class Parent implements Runnable {
        int incMean;
        int incDlta;
        int N;

        Parent(int Npar, int mean, int delta){
            this.incMean = mean;
            this.incDlta = delta;
            this.N = Npar;
        }

        public void run(){
            int money = 0;
            while (true) {
                try {
                    Thread.sleep(rnd(IncomeDelayMean, IncomeDelayDlta));
                    money = rnd(this.incMean, this.incDlta);
                    System.out.printf("parent %d adding %d money: ", this.N, money);
                    theBank.addMoney(money);
                    System.out.printf("p%d_done_adding_money\n", this.N);
                } catch (InterruptedException e){}
            }
        }
    }

    class Child  implements Runnable {
        int decMean;
        int decDlta;
        int N;

        Child(int Nch, int mean, int delta){
            this.decMean = mean;
            this.decDlta = delta;
            this.N = Nch;
        }

        public void run(){
            while (true) {
                int money = 0;
                try {
                    Thread.sleep(rnd(SpendDealyMean, SpendDelayDlta));
                    money = rnd(this.decMean, this.decDlta);
                    System.out.printf("child  %d taking %d money: ", this.N, money);
                    while (! theBank.checkAndTakeMoney(money)) {
                        Thread.sleep(waitDelay);
                    }
                    System.out.printf("c%d_done_taking_money\n", this.N);
                } catch (InterruptedException e){}
            }
        }
    }


    public static void main(String[] args) {
        FamilySim main = new FamilySim();

        System.out.println("starting parallel inc/dec simulation");
        System.out.printf("running with N parents = %d, N children = %d; mean salary = %d, mean use = %d\n",
                          NParents, NChildren, IncomeMean, SpendingMean);
        System.out.print("starting parent and child threads ..");
        for (int i=0; i<NParents; i++)  new Thread(main.new Parent(i+1, IncomeMean, IncomeDelta)).start();
        for (int i=0; i<NChildren; i++) new Thread(main.new Child (i+1, SpendingMean, SpendingDelta)).start();
        System.out.println(" done.");
    }
}
