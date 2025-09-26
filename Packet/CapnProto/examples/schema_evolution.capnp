# Schema evolution example for Cap'n Proto

@0xaaaaaaaaaaaaaaaa;  # Unique file ID

# Original schema (v1)
struct UserV1 {
  id @0 :UInt32;
  name @1 :Text;
  email @2 :Text;
}

# Evolved schema (v2) - adding new fields
struct UserV2 {
  id @0 :UInt32;
  name @1 :Text;
  email @2 :Text;
  phone @3 :Text;        # New field
  age @4 :UInt8;         # New field
  isActive @5 :Bool = true;  # New field with default
}

# Evolved schema (v3) - more changes
struct UserV3 {
  id @0 :UInt32;
  name @1 :Text;
  email @2 :Text;
  phone @3 :Text;
  age @4 :UInt8;
  isActive @5 :Bool = true;
  address @6 :Address;   # New nested struct
  tags @7 :List(Text);   # New list field
  metadata @8 :Data;     # New binary data field
}

struct Address {
  street @0 :Text;
  city @1 :Text;
  country @2 :Text;
  zipCode @3 :Text;
}

# Union for handling different schema versions
union UserData {
  v1 @0 :UserV1;
  v2 @1 :UserV2;
  v3 @2 :UserV3;
}

# Container that can handle any version
struct UserContainer {
  data @0 :UserData;
  schemaVersion @1 :UInt32 = 3;
  createdAt @2 :UInt64;
  updatedAt @3 :UInt64;
}

# Interface that supports schema evolution
interface UserService {
  # Create user (always uses latest schema)
  createUser @0 (user :UserV3) -> (id :UInt32);
  
  # Get user (returns in requested format)
  getUser @1 (id :UInt32, version :UInt32) -> (user :UserData);
  
  # Update user (handles schema migration)
  updateUser @2 (id :UInt32, user :UserData) -> (success :Bool);
  
  # List users with version support
  listUsers @3 (version :UInt32) -> (users :List(UserData));
}
