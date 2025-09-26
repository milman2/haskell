# Address book example for Cap'n Proto

@0x8b8b8b8b8b8b8b8b;  # Unique file ID

struct Date {
  year @0 :UInt16;
  month @1 :UInt8;
  day @2 :UInt8;
}

struct Person {
  id @0 :UInt32;
  name @1 :Text;
  email @2 :Text;
  phones @3 :List(PhoneNumber);
  lastSeen @4 :Date;
}

struct PhoneNumber {
  number @0 :Text;
  type @1 :PhoneType;
}

enum PhoneType {
  mobile @0;
  home @1;
  work @2;
}

struct AddressBook {
  people @0 :List(Person);
}

# RPC Interface
interface AddressBookService {
  addPerson @0 (person :Person) -> (id :UInt32);
  getPerson @1 (id :UInt32) -> (person :Person);
  listPeople @2 () -> (people :List(Person));
  searchPeople @3 (query :Text) -> (people :List(Person));
}
