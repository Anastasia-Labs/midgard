import { type ChildProcessWithoutNullStreams, spawn } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { createInterface, type Interface } from "node:readline";

import { blake2b } from "@noble/hashes/blake2.js";

import { CborReader } from "../src/cbor.js";

export type MockBlock = Readonly<{
  number: number;
  slot: number;
  hash: string;
  prev: string;
  raw: string;
}>;

type Reply = Readonly<Record<string, unknown>>;

/** The deterministic mock N2C node (native/cmd/l1-mock-node). */
export class MockNode {
  readonly #child: ChildProcessWithoutNullStreams;
  readonly #lines: Interface;
  readonly #waiting: Array<(line: string) => void> = [];
  readonly #directory: string;

  private constructor(
    child: ChildProcessWithoutNullStreams,
    directory: string,
    readonly socketPath: string,
  ) {
    this.#child = child;
    this.#directory = directory;
    this.#lines = createInterface({ input: child.stdout });
    this.#lines.on("line", (line) => this.#waiting.shift()?.(line));
  }

  static async start(
    binary: string,
    magic = 42,
    socketPath = MockNode.vacantSocket(),
  ): Promise<MockNode> {
    const directory = dirname(socketPath);
    const child = spawn(
      binary,
      ["--socket", socketPath, "--magic", String(magic)],
      {
        stdio: ["pipe", "pipe", "pipe"],
      },
    );
    child.stderr.resume();
    const node = new MockNode(child, directory, socketPath);
    const ready = JSON.parse(await node.#next()) as Reply;
    if (ready.ready !== true) throw new Error("mock node did not start");
    return node;
  }

  /** A socket path in a fresh directory with nothing listening yet. */
  static vacantSocket(): string {
    return join(mkdtempSync(join(tmpdir(), "l1nt-")), "node.socket");
  }

  #next(): Promise<string> {
    return new Promise((resolve) => this.#waiting.push(resolve));
  }

  async command(op: Readonly<Record<string, unknown>>): Promise<Reply> {
    const answer = this.#next();
    this.#child.stdin.write(`${JSON.stringify(op)}\n`);
    const reply = JSON.parse(await answer) as Reply;
    if (typeof reply.error === "string") throw new Error(reply.error);
    return reply;
  }

  async extend(count: number, branch = 0): Promise<MockBlock[]> {
    return (await this.command({ op: "extend", count, branch }))
      .blocks as MockBlock[];
  }

  async chain(): Promise<MockBlock[]> {
    return (await this.command({ op: "chain" })).blocks as MockBlock[];
  }

  async rollback(height: number): Promise<void> {
    await this.command({ op: "rollback", height });
  }

  async stats(): Promise<{
    requestNexts: number;
    connections: number;
    mempool: number;
    acquired: string[];
  }> {
    return (await this.command({ op: "stats" })) as never;
  }

  async close(): Promise<void> {
    const exited = new Promise((resolve) => this.#child.once("close", resolve));
    this.#child.stdin.end();
    await exited;
    rmSync(this.#directory, { recursive: true, force: true });
  }
}

/** blake2b-256 of the block's header item: the block hash. */
export const headerHash = (block: Uint8Array): string => {
  const reader = new CborReader(block);
  if (reader.readArrayHeader() === 0) throw new Error("empty block");
  return Buffer.from(blake2b(reader.readRaw(), { dkLen: 32 })).toString("hex");
};

export const hex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const within = async <T>(
  promise: Promise<T>,
  ms: number,
): Promise<T | "timeout"> => {
  let timer: NodeJS.Timeout | undefined;
  try {
    return await Promise.race([
      promise,
      new Promise<"timeout">((resolve) => {
        timer = setTimeout(() => resolve("timeout"), ms);
      }),
    ]);
  } finally {
    clearTimeout(timer);
  }
};
