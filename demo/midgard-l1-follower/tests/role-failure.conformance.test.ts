/**
 * The shared failure classification (`classifyRoleFailure`) against its
 * conformance rows, and the follow loop's adapters over it: a store failure
 * (`classifyFailure`, bounded by the loop's transient budget) and a
 * chain-sync stream failure (`classifyStreamFailure`, waited on without
 * bound) agree with the class of every row.
 */
import { describe, expect, it } from "vitest";

import {
  classifyFailure,
  classifyRoleFailure,
  classifyStreamFailure,
  type FailureClass,
  type RoleFailureClass,
} from "../src/index.js";
import { ROLE_FAILURE_ROWS } from "../src/testing/index.js";

/** What the follow loop does on each class: it retries contention like a
 * transient, and never exits the process, so a failure a restart may clear
 * counts toward `stuck` like an unknown one. */
const STORE: Readonly<Record<RoleFailureClass, FailureClass>> = {
  transient: "transient",
  contention: "transient",
  restart: "unknown",
  deterministic: "deterministic",
  unknown: "unknown",
};

describe("classifyRoleFailure", () => {
  it.each(ROLE_FAILURE_ROWS)("classifies $name as $expected", (row) => {
    expect(classifyRoleFailure(row.error())).toBe(row.expected);
  });
});

describe("the follow loop's adapters", () => {
  it.each(ROLE_FAILURE_ROWS)("a store failure: $name", (row) => {
    expect(classifyFailure(row.error())).toBe(STORE[row.expected]);
  });

  it.each(ROLE_FAILURE_ROWS)("a stream failure: $name", (row) => {
    expect(classifyStreamFailure(row.error())).toBe(
      STORE[row.expected] === "transient" ? "transient" : "unknown",
    );
  });
});
