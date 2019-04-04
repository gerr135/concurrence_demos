package test_buf_pkg is

    type Item_Type is new Integer;
    procedure printItem(I : Item_Type);

    maxItems : constant := 5;
    type Index is mod maxItems;
    maxCount : constant Index := Index(maxItems - 1);
    type ItemArray is array(Index) of Item_Type;

    type Buffer_Interface is synchronized interface;
    type Buffer_Access is access all Buffer_Interface'Class;
    procedure Put(Buf : in out Buffer_Interface; X : in  Item_Type) is abstract;
    procedure Get(Buf : in out Buffer_Interface; X : out Item_Type) is abstract;
    procedure PrintItem(Buf : in out Buffer_Interface'Class);

    protected type Buffer_Type is new Buffer_Interface with
        overriding
        entry Put(X : in  Item_Type);
        overriding
        entry Get(X : out Item_Type);
    private
        First, Last : Index := 0;
        buf : ItemArray;
    end Buffer_Type;


end test_buf_pkg;
