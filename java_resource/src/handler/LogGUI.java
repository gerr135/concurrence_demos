/*
 * A logger abstraction class with base/common functionality
 */

package handler;

import java.io.*;
// import LogAbstraction;

public class LogGUI extends Log {
    FamilyGUI gui;

    public LogGUI(FamilyGUI gui) {this.gui = gui;}

    @Override
    public void logBankAction(MoneyAction action, int amount) {
//         String actionStr = "";
//         switch (action) {
//             case INCREASE: actionStr = "_inc"; break;
//             case DECREASE: actionStr = "_dec"; break;
//         }
        gui.tBankAmount.setText(String.format("%d", amount));
    }


    @Override
    public void logParentAction(MoneyAction action, int N, int amount) {
        String actionStr = "";
        switch (action) {
            case INCREASE: actionStr = "_put"; break;
            case DECREASE: actionStr = "_get"; break;
        }
        gui.txtLog.append(String.format("p%d%s %d: ", N, actionStr, amount));
    }

    @Override
    public void logChildAction(MoneyAction action, int N, int amount) {
        String actionStr = "";
        switch (action) {
            case INCREASE: actionStr = "_put"; break;
            case DECREASE: actionStr = "_get"; break;
        }
        gui.txtLog.append(String.format("c%d%s %d: ", N, actionStr, amount));
    }

    @Override
    public void logParentDone(int N) {
        gui.txtLog.append(String.format("c%d_done\n", N));
    }

    @Override
    public void logChildDone(int N) {
        gui.txtLog.append(String.format("c%d_done\n", N));
    }

}
