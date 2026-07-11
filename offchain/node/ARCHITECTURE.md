# Overview

This repo is based on the Typescript midgard-node under `../demo`. More specifically, it is based on the formalized API of the midgard-node, which is initially derived from the Typescript implementation.

The server API was initially added in PR [#445](https://github.com/Anastasia-Labs/midgard/pull/445).

The database is largely based on the SQL files under `../demo/midgard-node/src/database/migrations/sql`. However, we define them in the persistent DSL to take advantage of the typed functions. It is important to keep these updated.

There is a test that ensures the persistent DSL defines the exact same schema (i.e no migrations are necessary) as what is already loaded in the database to be connected. This implies that one is responsible for **migrating the DB, using the raw SQL files, first**.

Sometimes, in order to make sure persistent DSL matches the raw SQL defined schema, it is necessary to make some mostly cosmetic changes to the raw SQL. Such changes are highlighted in PR [#455](https://github.com/Anastasia-Labs/midgard/pull/455).

# Continuing work

- Implement the necessary Codecs.

  Midgard has a lot of codecs associated with custom types, e.g `MidgardTxOutput`. These can be found under `../demo/midgard-core/src/codec`.

  The Haskell version will also have to define some of these and use them across the types.

  Currently, some of the codec types are implemented for wrapper types with instances under `DB.Types` and `Server.JSON.Types`. There is quite some duplication going on here. In reality, all of these types should be under Codec and they should all have JSON and PersistField instances that match Typescript side behavior.

  Note that Typescript needs many different types and functions for these but Haskell can achieve the same with one type and many instances.

- The persistent DSL is functional but WIP in terms of quality. It declares several fields as `ByteString`, when in reality it should declare them as specific Codec types. Fortunately, many fields are already well typed and using proper codec (defined `DB.Types`). Further well-typed field changes can be made based on these.
