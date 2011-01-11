ICustomer, IXML [Customer] 
IOrder, IXML [Order] 

[Customer] persitence -> IPersistence [Persitence]
[Customer] orders -> IOrder [Order]
[XML] convertCusts -> IXML [Customer]
[XML] convertOrders -> IXML [Order]

