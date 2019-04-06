import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.IOException;


class FirstThread implements Runnable {
    PipedOutputStream out;
    public FirstThread(PipedOutputStream out) {
        this.out = out;
        System.out.println("started FirstThread");
    }
    public void run() {
        try {
            for (int i=0; i<10; i++) {
                this.out.write((byte) i);
                System.out.println("1st sent: " + i);
            }
            this.out.close();
        } catch (IOException e) { e.printStackTrace(); }
    }
}

class SecondThread implements Runnable {
    PipedInputStream  in;
    PipedOutputStream out;
    public SecondThread(PipedInputStream  in, PipedOutputStream out) {
        this.in = in;
        this.out = out;
        System.out.println("started SecondThread");
    }
    public void run() {
        int read = -1;
        try {
            while ((read = this.in.read()) != -1) {
                System.out.println("2nd read: " + read);
                this.out.write((byte) read + 10);
                System.out.println("2nd sent: " + (read+10));
            }
            this.in.close();
            this.out.close();
        } catch (IOException e) { e.printStackTrace(); }
    }
}

class ThirdThread implements Runnable {
    PipedInputStream  in;
    public ThirdThread(PipedInputStream  in) {
        this.in = in;
        System.out.println("started ThirdThread");
    }
    public void run() {
        int read = -1;
        try {
            while ((read = this.in.read()) != -1) {
                System.out.println("3rd received: " + read);
            }
            this.in.close();
        } catch (IOException e) { e.printStackTrace(); }
    }
}

public class pipe_demo {
    public static void main(String[] args) {
        try {
            PipedInputStream  is1 = new PipedInputStream();
            PipedOutputStream os1 = new PipedOutputStream(is1);
            PipedInputStream  is2 = new PipedInputStream();
            PipedOutputStream os2 = new PipedOutputStream(is2);
            System.out.println("Starting threads");
            FirstThread  first  = new FirstThread(os1);
            SecondThread second = new SecondThread(is1, os2);
            ThirdThread  third  = new ThirdThread(is2);
            new Thread(first).start();
            new Thread(second).start();
            new Thread(third).start();
        } catch (IOException e) { e.printStackTrace(); }
    }
}
