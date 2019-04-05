/*
 * A logger abstraction class with base/common functionality
 */

package handler;

import java.io.*;
// import LogAbstraction;

public class LogCmdline extends Log {

    @Override
    public void logBankAction(MoneyAction action, int amount) {
        String actionStr = "";
        switch (action) {
            case INCREASE: actionStr = "_inc"; break;
            case DECREASE: actionStr = "_dec"; break;
        }
        System.out.printf("bank%s(money=%d);  ", actionStr, amount);
    }


    @Override
    public void logParentAction(MoneyAction action, int N, int amount) {
        String actionStr = "";
        switch (action) {
            case INCREASE: actionStr = "_put"; break;
            case DECREASE: actionStr = "_get"; break;
        }
        System.out.printf("p%d%s %d: ", N, actionStr, amount);
    }

    @Override
    public void logChildAction(MoneyAction action, int N, int amount) {
        String actionStr = "";
        switch (action) {
            case INCREASE: actionStr = "_put"; break;
            case DECREASE: actionStr = "_get"; break;
        }
        System.out.printf("c%d%s %d: ", N, actionStr, amount);
    }

    @Override
    public void logParentDone(int N) {
        System.out.printf("c%d_done\n", N);
    }

    @Override
    public void logChildDone(int N) {
        System.out.printf("c%d_done\n", N);
    }

}
