import { openSqliteFactStore } from "../../src/index.js";
import type { FollowerProjection } from "../../src/shadow/index.js";
import type { ForkRunOptions } from "../../src/testing/index.js";
import {
  FIXTURE_DERIVATION,
  FIXTURE_TABLES,
  fixtureMigrations,
} from "./fixture.js";

/** The property test's sample D-t tables, plugged in as a projection. */
export const FIXTURE_PROJECTION: FollowerProjection = {
  name: "fixture",
  temporalTables: FIXTURE_TABLES,
  migrations: fixtureMigrations,
  derivations: [FIXTURE_DERIVATION],
};

/** A fresh in-memory SQLite store under test. */
export const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

/** The rollback depth bound the simulator suites run with. */
export const SIM_K = 6;
