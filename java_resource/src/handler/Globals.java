/**
 * Globals (top-level) depo.
 * This is to keep the high-level vars (Bank, Family, etc) in a clear and visible place.
 * Using globals here to avoid having to extensively touch the GUI code, as well to avoid code duplication
 * as with the GUI variant the "main loop" is with the form, so it would normally hold the top-level vars,
 * but we are requested to do 2 versions of the same that only differe by little deep inside implementation details..
 *
 * Instead we keep the form as a (common for all) class and have a small separate main proc
 * initializing proper Bank and logger variants..
 *
 * NOTE: some of these vars could be held in Params class, but not all (esp the logger)
 * as this would create circular deps (which will not even be evident in Java, where import serves as a
 * visibility statement, not a real import as in C/C++, more like "use" (not "with") in Ada -
 * so while we may be able get away with it here, it would cause a great headache in a big project).
 * So we just group them all in a single logical place..
 */
package handler;

public class Globals {
    public static bank.Abstraction theBank;
    public static family.Family    theFamily;
    public static Log logger;
}
