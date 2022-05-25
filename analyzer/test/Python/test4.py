#Represents the node of list.    
class Node:    
  def __init__(self,data):    
    self.data = data;    
    self.next = None;    
     
class CreateList:    
    #Declaring head and tail pointer as null.    
    def __init__(self):    
      self.head = Node(None);    
      self.tail = Node(None);    
      self.head.next = self.tail;    
      self.tail.next = self.head;    
      self.size = 0;    
        
    #This function will add the new node to the list.    
    def add(self,data):    
       newNode = Node(data);    
       #Checks if the list is empty.    
       if self.head.data is None:    
         #If list is empty, both head and tail would point to new node.    
         self.head = newNode;    
         self.tail = newNode;    
         newNode.next = self.head;    
       else:    
         #tail will point to new node.    
         self.tail.next = newNode;    
         #New node will become new tail.    
         self.tail = newNode;    
         #Since, it is circular linked list tail will points to head.    
         self.tail.next = self.head;    
       #Size will count the number of element in the list    
       self.size = self.size+1;    
        
    #This function will add the new node at the middle of the list.    
    def addInMid(self,data):    
        newNode = Node(data);    
        #Checks if the list is empty.    
        if(self.head == None):    
            #If list is empty, both head and tail would point to new node.    
            self.head = newNode;    
            self.tail = newNode;    
            newNode.next = self.head;    
        else:    
            #Store the mid-point of the list    
            count = (self.size//2) if (self.size % 2 == 0) else ((self.size+1)//2);    
            #temp will point to head    
            temp = self.head;    
            for i in range(0,count):    
                #Current will point to node previous to temp.    
                current = temp;    
                #Traverse through the list till the middle of the list is reached    
                temp = temp.next;    
            #current will point to new node    
            current.next = newNode;    
            #new node will point to temp    
            newNode.next = temp;    
        self.size = self.size+1;    
            
    #Displays all the nodes in the list    
    def display(self):    
      current = self.head;    
      if self.head is None:    
        print("List is empty");    
        return;    
      else:    
          #Prints each node by incrementing pointer.    
          print(current.data),    
          while(current.next != self.head):    
              current = current.next;    
              print(current.data),    
        
     
class CircularLinkedList:    
    cl = CreateList();    
    #Adds data to the list    
    cl.add(1);    
    cl.add(2);    
    cl.add(3);    
    cl.add(4);    
    print("Original list: ");    
    cl.display();    
    #Inserting node '5' in the middle    
    cl.addInMid(5);    
    print("\nUpdated List: ");    
    cl.display();    
    #Inserting node '6' in the middle    
    cl.addInMid(6);    
    print("\nUpdated List: ");    
    cl.display();   