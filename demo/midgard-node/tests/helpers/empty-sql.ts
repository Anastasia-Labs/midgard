import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { vi } from "vitest";

/** A SQL client over a schema-complete database with no rows. */
export const emptySql = Object.assign(
  ((..._args: readonly unknown[]) =>
    Effect.succeed([])) as unknown as SqlClient.SqlClient,
  {
    array: vi.fn((values: readonly unknown[]) => values),
    in: vi.fn((values: readonly unknown[]) => values),
    literal: (text: string) => text,
    withTransaction: <A, E, R>(effect: Effect.Effect<A, E, R>) => effect,
  },
) as unknown as SqlClient.SqlClient;
