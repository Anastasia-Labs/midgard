import { createHash } from "node:crypto";
import { once } from "node:events";
import { createReadStream, createWriteStream } from "node:fs";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import {
  type OpenLoopCorpusRow,
  type OpenLoopCorpusShape,
  parseOpenLoopCorpusLine,
} from "@/commands/stress-open-loop.js";

export type CorpusIndexEntry = {
  readonly corpusSliceId: string;
  readonly planShape: OpenLoopCorpusShape;
  readonly chainId: string;
  readonly startByteOffset: number;
  readonly endByteOffset: number;
  readonly rowCount: number;
};

export type AssembleCorpusResult = {
  readonly corpusPath: string;
  readonly indexPath: string;
  readonly rowCount: number;
  readonly chainCount: number;
  readonly corpusSha256: string;
  readonly indexSha256: string;
  readonly indexEntries: readonly CorpusIndexEntry[];
};

export type ShardRowWriter = {
  readonly writeRows: (rows: readonly OpenLoopCorpusRow[]) => Promise<void>;
  readonly close: () => Promise<{
    readonly rowCount: number;
    readonly sha256: string;
  }>;
};

const sha256File = async (path: string): Promise<string> =>
  createHash("sha256")
    .update(await readFile(path))
    .digest("hex");

async function* readLines(path: string): AsyncGenerator<string> {
  let carry = "";
  for await (const chunk of createReadStream(path, { encoding: "utf8" })) {
    carry += chunk;
    let newlineIndex = carry.indexOf("\n");
    while (newlineIndex >= 0) {
      const line = carry.slice(0, newlineIndex).replace(/\r$/u, "");
      carry = carry.slice(newlineIndex + 1);
      if (line.trim().length > 0) {
        yield line;
      }
      newlineIndex = carry.indexOf("\n");
    }
  }
  if (carry.trim().length > 0) {
    yield carry.replace(/\r$/u, "");
  }
}

const parseRowHeader = (
  line: string,
): Pick<
  OpenLoopCorpusRow,
  "senderWalletId" | "corpusSliceId" | "planShape"
> => {
  const parsed = parseOpenLoopCorpusLine(line, 1);
  return {
    senderWalletId: parsed.senderWalletId,
    corpusSliceId: parsed.corpusSliceId,
    planShape: parsed.planShape,
  };
};

export const writeShardRows = async (
  path: string,
  rows: readonly OpenLoopCorpusRow[],
): Promise<{ readonly rowCount: number; readonly sha256: string }> => {
  const writer = await createShardRowWriter(path);
  await writer.writeRows(rows);
  return writer.close();
};

export const createShardRowWriter = async (
  path: string,
): Promise<ShardRowWriter> => {
  await mkdir(dirname(path), { recursive: true });
  const hash = createHash("sha256");
  const output = createWriteStream(path, {
    encoding: "utf8",
    flags: "w",
  });
  let rowCount = 0;
  return {
    writeRows: async (rows) => {
      for (const row of rows) {
        const bytes = Buffer.from(`${JSON.stringify(row)}\n`, "utf8");
        hash.update(bytes);
        if (!output.write(bytes)) {
          await once(output, "drain");
        }
        rowCount += 1;
      }
    },
    close: async () => {
      output.end();
      await once(output, "finish");
      return {
        rowCount,
        sha256: hash.digest("hex"),
      };
    },
  };
};

export const assembleCorpusShards = async ({
  shardPaths,
  corpusPath,
  indexPath,
}: {
  readonly shardPaths: readonly string[];
  readonly corpusPath: string;
  readonly indexPath: string;
}): Promise<AssembleCorpusResult> => {
  await mkdir(dirname(corpusPath), { recursive: true });
  await mkdir(dirname(indexPath), { recursive: true });
  const corpusHash = createHash("sha256");
  const corpusOut = createWriteStream(corpusPath, {
    encoding: "utf8",
    flags: "w",
  });
  let byteOffset = 0;
  let rowCount = 0;
  let currentRun:
    | {
        readonly corpusSliceId: string;
        readonly planShape: OpenLoopCorpusShape;
        readonly chainId: string;
        readonly startByteOffset: number;
        rowCount: number;
      }
    | undefined;
  const indexEntries: CorpusIndexEntry[] = [];

  const closeRun = (): void => {
    if (currentRun === undefined) {
      return;
    }
    indexEntries.push({
      corpusSliceId: currentRun.corpusSliceId,
      planShape: currentRun.planShape,
      chainId: currentRun.chainId,
      startByteOffset: currentRun.startByteOffset,
      endByteOffset: byteOffset,
      rowCount: currentRun.rowCount,
    });
    currentRun = undefined;
  };

  for (const shardPath of shardPaths) {
    for await (const line of readLines(shardPath)) {
      const row = parseRowHeader(line);
      if (
        currentRun === undefined ||
        currentRun.chainId !== row.senderWalletId ||
        currentRun.corpusSliceId !== row.corpusSliceId ||
        currentRun.planShape !== row.planShape
      ) {
        closeRun();
        currentRun = {
          corpusSliceId: row.corpusSliceId,
          planShape: row.planShape,
          chainId: row.senderWalletId,
          startByteOffset: byteOffset,
          rowCount: 0,
        };
      }
      const bytes = Buffer.from(`${line}\n`, "utf8");
      corpusHash.update(bytes);
      if (!corpusOut.write(bytes)) {
        await once(corpusOut, "drain");
      }
      byteOffset += bytes.length;
      rowCount += 1;
      currentRun.rowCount += 1;
    }
  }
  closeRun();
  corpusOut.end();
  await once(corpusOut, "finish");

  const indexContent = indexEntries
    .map((entry) => JSON.stringify(entry))
    .join("\n");
  await writeFile(
    indexPath,
    indexEntries.length === 0 ? "" : `${indexContent}\n`,
    "utf8",
  );
  return {
    corpusPath,
    indexPath,
    rowCount,
    chainCount: indexEntries.length,
    corpusSha256: corpusHash.digest("hex"),
    indexSha256: await sha256File(indexPath),
    indexEntries,
  };
};
