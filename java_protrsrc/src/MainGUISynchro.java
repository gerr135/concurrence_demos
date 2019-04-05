/*
 * Main proc, GUI version
 */

import handler.*;
import family.*;
import bank.*;

public class MainGUISynchro extends javax.swing.JFrame {


    public static void main(String args[]) {
        /* Create and display the form */
        MainGUISynchro main = new MainGUISynchro();
        FamilyGUI gui = new FamilyGUI();
        Globals.logger  = new LogGUI(gui);
        Globals.theBank = new bank.Synchro(Globals.logger);
        Globals.theFamily = new family.Family();
        // construction done, set init values and start the show..

        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() { gui.setVisible(true); }
        });

    }
}

