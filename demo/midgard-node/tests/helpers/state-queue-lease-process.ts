import { Effect } from "effect";

import { StateQueueMutationLeasesDB } from "../../src/database/index.js";
import { Database } from "../../src/services/database.js";

const mode = process.argv[2];
const holder = process.argv[3] ?? `lease-probe-${process.pid.toString()}`;

const write = (event: Readonly<Record<string, unknown>>): void => {
  process.stdout.write(`${JSON.stringify(event)}\n`);
};

const program = Effect.gen(function* () {
  if (mode !== "hold" && mode !== "contend") {
    throw new Error(`Unknown lease probe mode: ${String(mode)}`);
  }
  const result = yield* StateQueueMutationLeasesDB.tryWithLease(
    holder,
    () =>
      mode === "hold"
        ? Effect.sync(() => write({ event: "acquired", holder })).pipe(
            Effect.andThen(Effect.sleep("2 seconds")),
          )
        : Effect.succeed("entered"),
    { ttlMs: 10_000, renewIntervalMs: 1_000 },
  );
  write({
    event: "result",
    mode,
    holder,
    result: result._tag,
    activeHolder:
      result._tag === "Busy"
        ? result.activeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER]
        : undefined,
  });
});

void Effect.runPromise(program.pipe(Effect.provide(Database.layer))).then(
  () => {
    process.stdout.write("", () => process.exit(0));
  },
  (error: unknown) => {
    process.stderr.write(
      `${error instanceof Error ? error.stack : String(error)}\n`,
      () => process.exit(1),
    );
  },
);
