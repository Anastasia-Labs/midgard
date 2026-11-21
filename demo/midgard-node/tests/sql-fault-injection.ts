import { SqlClient } from "@effect/sql";
import { SqlError } from "@effect/sql/SqlError";
import { Effect } from "effect";

/** One tagged-template statement as the client received it. */
export type SqlStatementCall = {
  readonly text: string;
  readonly values: readonly unknown[];
};

const isTemplateCall = (
  args: readonly unknown[],
): args is readonly [TemplateStringsArray, ...unknown[]] =>
  Array.isArray(args[0]) && "raw" in (args[0] as object);

/**
 * `sql` with every tagged-template statement `shouldFail` selects failing as
 * a dropped connection would. Identifier and helper calls (`sql("t")`,
 * `sql.in`, `sql.withTransaction`) pass through untouched.
 */
export const withFailingStatements = (
  sql: SqlClient.SqlClient,
  shouldFail: (call: SqlStatementCall) => boolean,
): SqlClient.SqlClient =>
  new Proxy(sql, {
    apply(target, thisArg, args: unknown[]) {
      if (isTemplateCall(args)) {
        const [strings, ...values] = args;
        if (shouldFail({ text: strings.join("?"), values }))
          return Effect.fail(
            new SqlError({
              cause: new Error("Connection terminated unexpectedly"),
              message: "Injected statement failure",
            }),
          );
      }
      return Reflect.apply(target as never, thisArg, args);
    },
  });
