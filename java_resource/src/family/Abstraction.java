/**
 * base code for parallel sim, no gui
 */



public class FamilySimSynchro {

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


    class MoneyBank {
        int money;
        MoneyBank() {this.money = 0;}

        // this one requires only serialized access, no checks
        public synchronized void put (int amount) {
            this.money += amount;
            this.notifyAll();
            System.out.printf("bank_inc(money=%d);  ", this.money);
        }

        // Taking resources is more tricky as we have a condition (money >=0)
        // Java does not have guarded task entries (as Ada does, but well, the same goes
        // for C and all other major languages), so we have to implement this ourselves.
        //
        // we make two methods. non-blocking chackAndGet and blocking waitAndGet
        //
        // The Non-blocking checkAndGet mimics the "family routine"
        // each child runs up to check if there is enough money, and then takes or runs away,
        // to return at a later point. This allows clients to keep doing useful stuff if they want..
        // NOTE: potentially we can starve a child, but we are not asked to prevent starvation here.
        // In fact, clients can just check at regular intervals (the same for every child),
        // which will prevent starvation in all but few very special cases..
        public synchronized boolean checkAndGet(int amount){
            // returns immediately.
            // check if enough money is present, allocate resources and return true if yes,
            // return false if no
            if (amount <= this.money) {
                this.money -= amount;
                System.out.printf("bank_dec(money=%d);  ", this.money);
                return true;
            } else {return false;}
        }

        // The blocking method. Makes the calling thread wait until enough money is present.
        // Uses monitors internally to sleep instead of busy-waiting.
        // Can be called directly without extra logic..
        public synchronized void waitAndGet(int amount) throws InterruptedException {
            // first we check..
            // (in cycle, because each time we wait for deposit, but it may not be enough)
            while (amount > this.money) {
                // wait until deposit and recheck
                this.wait();
            }
            this.money -= amount;
            System.out.printf("bank_dec(money=%d);  ", this.money);
        }
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
                    System.out.printf("p%d_put %d: ", this.N, money);
                    theBank.put(money);
                    System.out.printf("p%d_done\n", this.N);
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
                    System.out.printf("c%d_get %d: ", this.N, money);
                    theBank.waitAndGet(money);
                    System.out.printf("c%d_done\n", this.N);
                } catch (InterruptedException e){}
            }
        }
    }


    public static void main(String[] args) {
        FamilySimSynchro main = new FamilySimSynchro();

        System.out.println("starting parallel inc/dec simulation");
        System.out.printf("running with N parents = %d, N children = %d; mean salary = %d, mean use = %d\n",
                          NParents, NChildren, IncomeMean, SpendingMean);
        System.out.print("starting parent and child threads ..");
        for (int i=0; i<NParents; i++)  new Thread(main.new Parent(i+1, IncomeMean, IncomeDelta)).start();
        for (int i=0; i<NChildren; i++) new Thread(main.new Child (i+1, SpendingMean, SpendingDelta)).start();
        System.out.println(" done.");
    }
}
