import { Effect } from "effect";

import { makeSeededAdversarialMpfCorpusBlock } from "@/commands/mpf-replay.js";

const seed = Number.parseInt(process.env.MPF_CORPUS_SEED ?? "1337", 10);

void Effect.runPromise(makeSeededAdversarialMpfCorpusBlock(seed)).then(
  (block) => process.stdout.write(`${JSON.stringify(block)}\n`),
  (error: unknown) => {
    process.stderr.write(
      `${error instanceof Error ? error.stack : String(error)}\n`,
    );
    process.exitCode = 1;
  },
);
