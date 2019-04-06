/*
 * A logger abstraction class with base/common functionality
 */

package handler;

public abstract class Log {

    public enum MoneyAction {INCREASE,DECREASE}

    public abstract void logBankAction  (MoneyAction action, int amount);
    public abstract void logParentAction(MoneyAction action, int N, int amount);
    public abstract void logChildAction (MoneyAction action, int N, int amount);
    public abstract void logParentDone(int N);
    public abstract void logChildDone (int N);

}
