namespace tutorial;

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

struct Date {
  year @0 :UInt16;
  month @1 :UInt8;
  day @2 :UInt8;
}

interface AddressBookService {
  addPerson(Person person) -> UInt32;
  getPerson(UInt32 id) -> Person;
  listPeople() -> List(Person);
}

const MAX_PHONES = 10;