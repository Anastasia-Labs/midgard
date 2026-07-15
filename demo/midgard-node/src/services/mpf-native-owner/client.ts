import { type MessagePort } from "node:worker_threads";

import {
  type NativeMpfApplyResult,
  type NativeMpfGenerationHandle,
  type NativeMpfOwnerClient,
} from "./protocol.js";

type Pending = {
  readonly resolve: (value: unknown) => void;
  readonly reject: (error: Error) => void;
  readonly timer: NodeJS.Timeout;
};

export class NativeMpfWorkerPortClient implements NativeMpfOwnerClient {
  private requestId = 0;
  private readonly pending = new Map<number, Pending>();

  public constructor(
    private readonly port: MessagePort,
    private readonly timeoutMs = 30_000,
  ) {
    port.on("message", (message: unknown) => this.onMessage(message));
    port.once("close", () =>
      this.failAll(new Error("Native MPF worker port closed")),
    );
    port.start();
  }

  public fork(baseRoot: string): Promise<NativeMpfGenerationHandle> {
    return this.request("fork", {
      baseRoot,
    }) as Promise<NativeMpfGenerationHandle>;
  }

  public applyEvents(
    handle: NativeMpfGenerationHandle,
    eventLog: Uint8Array,
  ): Promise<NativeMpfApplyResult> {
    return this.request("applyEvents", {
      handle,
      eventLog,
    }) as Promise<NativeMpfApplyResult>;
  }

  public async discard(handle: NativeMpfGenerationHandle): Promise<void> {
    await this.request("discard", { handle });
  }

  /** Transfers the generation lease to the already-durable submission journal. */
  public async retainForJournal(
    handle: NativeMpfGenerationHandle,
  ): Promise<void> {
    await this.request("retainForJournal", { handle });
  }

  public close(): void {
    this.port.close();
    this.failAll(new Error("Native MPF worker client closed"));
  }

  private request(
    method: string,
    fields: Record<string, unknown>,
  ): Promise<unknown> {
    const requestId = ++this.requestId;
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(requestId);
        reject(
          new Error(`Native MPF worker request timed out: method=${method}`),
        );
      }, this.timeoutMs);
      this.pending.set(requestId, { resolve, reject, timer });
      this.port.postMessage({ requestId, method, ...fields });
    });
  }

  private onMessage(message: unknown): void {
    if (
      typeof message !== "object" ||
      message === null ||
      !("requestId" in message) ||
      !("ok" in message)
    ) {
      this.failAll(new Error("Native MPF worker response is malformed"));
      this.port.close();
      return;
    }
    const response = message as Record<string, unknown>;
    const requestId = Number(response.requestId);
    const pending = this.pending.get(requestId);
    if (pending === undefined) {
      this.failAll(
        new Error(
          `Native MPF worker response is unsolicited: ${requestId.toString()}`,
        ),
      );
      this.port.close();
      return;
    }
    clearTimeout(pending.timer);
    this.pending.delete(requestId);
    if (response.ok === true) pending.resolve(response.value);
    else pending.reject(new Error(String(response.error)));
  }

  private failAll(error: Error): void {
    for (const pending of this.pending.values()) {
      clearTimeout(pending.timer);
      pending.reject(error);
    }
    this.pending.clear();
  }
}
