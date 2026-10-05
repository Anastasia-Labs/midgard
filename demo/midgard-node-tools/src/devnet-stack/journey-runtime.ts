import { join, resolve } from "node:path";

import { walletFromSeed } from "@lucid-evolution/lucid";

import type { DeployContext } from "./deploy.js";
import {
  execLogged,
  type ExecResult,
  lastJsonValue,
  requireSuccess,
} from "./exec.js";
import type { TestAsset } from "./funding.js";
import { recordedHistoryGenesisPin } from "./history-pin.js";
import { type UserRole, walletInfo } from "./identities.js";
import { Journal } from "./journal.js";
import {
  awaitEarlierAttempts,
  CliFailure,
  type CliRequest,
  type CliRunner,
  definitiveCliFailure,
  describeError,
  FatalJourneyError,
  firstLine,
  JourneyDeadlineError,
  sleep,
  SUBMISSION_MARKER_ENV,
  SUBMISSION_RUN_ENV,
} from "./journey-cli.js";
import {
  DEFAULT_DEADLINES,
  type JourneyDeadlines,
  type JourneyOptions,
  type SettlementWatch,
} from "./journey-runtime.options.js";
import {
  ADA,
  addValues,
  type Json,
  sameValue,
  toValue,
  type Value,
} from "./journey-values.js";
import { servicePorts } from "./layout.js";
import { type HubOracleOneShot, nodeEnvironment } from "./node-env.js";

export {
  DEFAULT_DEADLINES,
  type JourneyDeadlines,
  type JourneyOptions,
  type SettlementWatch,
};

/**
 * The journey's plumbing: the node CLI with retries under a stable
 * submission id, HTTP reads that treat an unavailable dependency as "not
 * yet", deadline-bounded polling and journaled phases.
 */
export class JourneyRuntime {
  readonly journal: Journal;
  readonly nodeUrl: string;
  readonly deadlines: JourneyDeadlines;
  readonly wait: (ms: number) => Promise<void>;
  protected readonly logDir: string;
  private readonly runCli: CliRunner;
  private readonly fetch: typeof fetch;
  private readonly pollMs: number;
  private readonly retryInitialMs: number;
  private readonly retryMaxMs: number;
  private readonly cliTimeoutMs: number;
  private readonly httpTimeoutMs: number;
  protected readonly resubmitAfterMs: number;
  private readonly logLine: (message: string) => void;

  constructor(
    readonly context: DeployContext,
    readonly oneShot: HubOracleOneShot,
    readonly assets: readonly TestAsset[],
    options: JourneyOptions = {},
  ) {
    this.journal = new Journal(join(context.layout.journeyDir, "journey.json"));
    this.nodeUrl = `http://127.0.0.1:${servicePorts(context.run).nodeHttp}`;
    this.logDir = join(context.layout.journeyDir, "logs");
    this.deadlines = { ...DEFAULT_DEADLINES, ...options.deadlines };
    this.wait = options.sleep ?? sleep;
    this.logLine =
      options.log ??
      ((message) =>
        console.log(`${new Date().toISOString()} journey: ${message}`));
    this.fetch = options.fetch ?? ((input, init) => fetch(input, init));
    this.pollMs = options.pollMs ?? 5_000;
    this.retryInitialMs = options.retryInitialMs ?? 5_000;
    this.retryMaxMs = options.retryMaxMs ?? 60_000;
    this.cliTimeoutMs = options.cliTimeoutMs ?? 600_000;
    this.httpTimeoutMs = options.httpTimeoutMs ?? 30_000;
    this.resubmitAfterMs = options.resubmitAfterMs ?? 120_000;
    this.runCli = options.runCli ?? ((request) => this.nodeCli(request));
  }

  log(message: string) {
    this.logLine(message);
  }

  address(user: UserRole) {
    return walletInfo(this.context.identities.seeds[user]).address;
  }

  /** A fresh L1 address the user owns, one per withdrawal. */
  payoutAddress(user: UserRole, index: number) {
    return walletFromSeed(this.context.identities.seeds[user], {
      network: "Custom",
      accountIndex: 100 + index,
    }).address;
  }

  unit(label: string) {
    const asset = this.assets.find((candidate) => candidate.label === label);
    if (asset === undefined) throw new Error(`unknown test asset ${label}`);
    return `${asset.policyId}${asset.assetNameHex}`;
  }

  value(ada: bigint, tokens: Record<string, bigint> = {}): Value {
    return addValues(
      { lovelace: ada * ADA },
      Object.fromEntries(
        Object.entries(tokens).map(([label, amount]) => [
          this.unit(label),
          amount,
        ]),
      ),
    );
  }

  /** Identifies this run's CLI attempts among every process on the machine. */
  get runKey() {
    return resolve(this.context.layout.journeyDir);
  }

  /** The same scenario id always resumes the same submission. */
  submissionId(id: string) {
    return `${this.context.run.runId}-${id}`;
  }

  protected assetSpecs(value: Value) {
    return Object.entries(value)
      .filter(([unit]) => unit !== "lovelace")
      .map(
        ([unit, amount]) => `${unit.slice(0, 56)}.${unit.slice(56)}:${amount}`,
      );
  }

  /** The production runner: the node CLI with the run's command environment. */
  private async nodeCli(request: CliRequest): Promise<ExecResult> {
    if (request.submissionId !== undefined)
      await awaitEarlierAttempts(
        this.runKey,
        request.submissionId,
        request.timeoutMs,
        (message) => this.log(message),
        this.wait,
      );
    return execLogged(process.execPath, ["dist/index.js", ...request.args], {
      cwd: this.context.layout.nodeRoot,
      env: {
        ...nodeEnvironment({
          ...this.context,
          oneShot: this.oneShot,
          historyGenesisPin: recordedHistoryGenesisPin(this.context.layout),
          role: "command",
        }),
        ...(request.user === undefined
          ? {}
          : { USER_SEED_PHRASE: this.context.identities.seeds[request.user] }),
        ...(request.submissionId === undefined
          ? {}
          : {
              [SUBMISSION_RUN_ENV]: this.runKey,
              [SUBMISSION_MARKER_ENV]: request.submissionId,
            }),
      },
      logDir: this.logDir,
      label: request.label,
      timeoutMs: request.timeoutMs,
    });
  }

  /**
   * One CLI attempt. A failure no retry can repair (definitiveCliFailure) is
   * a FatalJourneyError; any other failure to produce a JSON result is a
   * CliFailure.
   */
  protected async attempt(
    user: UserRole | undefined,
    args: readonly string[],
    label: string,
    submissionId?: string,
  ): Promise<{ result: Json; transcript: string }> {
    let run: ExecResult;
    try {
      run = await this.runCli({
        user,
        args,
        label,
        timeoutMs: this.cliTimeoutMs,
        ...(submissionId === undefined ? {} : { submissionId }),
      });
    } catch (error) {
      throw new CliFailure(
        `${label} could not run: ${describeError(error)}`,
        undefined,
      );
    }
    const definitive =
      run.code === 0
        ? undefined
        : definitiveCliFailure(`${run.stdout}\n${run.stderr}`);
    if (definitive !== undefined)
      throw new FatalJourneyError(
        `${label} failed in a way no retry can repair (${definitive}); transcript ${run.log}`,
      );
    try {
      return {
        result: lastJsonValue(requireSuccess(run, label).stdout) as Json,
        transcript: run.log,
      };
    } catch (error) {
      throw new CliFailure(`${label}: ${describeError(error)}`, run.log);
    }
  }

  async cli(
    user: UserRole | undefined,
    args: readonly string[],
    label: string,
  ) {
    return (await this.attempt(user, args, label)).result;
  }

  /**
   * Runs a submission until it succeeds, always under `submissionId` with the
   * same arguments, with capped backoff until the submit deadline. A failure
   * no retry can repair ends it at once (see `attempt`); insufficient funds,
   * no free nonce and an unavailable node stay retryable.
   */
  protected async submit(
    what: string,
    submissionId: string,
    user: UserRole,
    args: readonly string[],
    label: string,
  ) {
    const deadline = Date.now() + this.deadlines.submitMs;
    let backoff = this.retryInitialMs;
    for (let attempt = 1; ; attempt += 1) {
      try {
        return await this.attempt(user, args, label, submissionId);
      } catch (error) {
        if (!(error instanceof CliFailure)) throw error;
        const remaining = deadline - Date.now();
        if (remaining <= 0)
          throw new JourneyDeadlineError(
            `${what} did not complete within ${this.deadlines.submitMs / 1000} s under submission id ${submissionId}; transcript ${error.transcript ?? "none"}: ${error.message}`,
          );
        const delay = Math.min(backoff, remaining);
        this.log(
          `${what} attempt ${attempt} failed (${firstLine(error.message)}); retrying under submission id ${submissionId} in ${delay / 1000} s; transcript ${error.transcript ?? "none"}`,
        );
        await this.wait(delay);
        backoff = Math.min(backoff * 2, this.retryMaxMs);
      }
    }
  }

  /**
   * One HTTP request. A refused or reset connection, a timeout, a 5xx (bar
   * an accepted 503, the body of an unready /readyz) or a body that is not
   * JSON throws a plain Error, which every wait treats as "not yet".
   */
  async http(
    path: string,
    base = this.nodeUrl,
    accept503 = false,
  ): Promise<{ status: number; body: Json }> {
    let status: number;
    let text: string;
    try {
      const response = await this.fetch(`${base}${path}`, {
        signal: AbortSignal.timeout(this.httpTimeoutMs),
      });
      status = response.status;
      text = await response.text();
    } catch (error) {
      throw new Error(`GET ${path}: ${describeError(error)}`);
    }
    if (status >= 500 && !(accept503 && status === 503))
      throw new Error(`GET ${path} answered ${status}: ${text.slice(0, 300)}`);
    try {
      return { status, body: JSON.parse(text) as Json };
    } catch {
      throw new Error(
        `GET ${path} answered ${status} with a non-JSON body: ${text.slice(0, 300)}`,
      );
    }
  }

  async pipeline() {
    return (await this.http("/pipeline-status")).body;
  }

  /**
   * The settlement job of an event whose last attempt failed, described with
   * the worker's error, as /pipeline-status lists it; null when it is not
   * failing, undefined when the status cannot be read.
   */
  async failingSettlementJob(
    kind: string,
    eventId: string,
  ): Promise<string | null | undefined> {
    let status: Json;
    try {
      status = await this.pipeline();
    } catch {
      return undefined;
    }
    const job = (
      ((status.settlement as Json | undefined)?.failingJobs ?? []) as Json[]
    ).find(
      (candidate) => candidate.kind === kind && candidate.eventId === eventId,
    );
    return job === undefined
      ? null
      : `settlement job ${kind} ${eventId} is in phase ${String(job.phase)} after ${String(job.failures)} failed attempts: ${String(job.lastError)}`;
  }

  /**
   * The node's settlement health as "<state>: <first line of detail>", read
   * from /readyz whether or not the node is ready. Throws a FatalJourneyError
   * once, at every read for settlementErrorMs, the awaited job has kept
   * failing or the worker has kept failing; each is timed on its own.
   *
   * The awaited job is timed from its own record: `failing`, the job as
   * /pipeline-status lists it (see failingSettlementJob), stays set through
   * the job's backoff, whatever the worker reports for other jobs between
   * its turns. Its pending body's failing reconcile is not recorded on the
   * job, so a health 'error' naming `eventId` (the node labels a job's
   * failures with its kind and event id) counts too. Another job's
   * persistent error never ends the wait: that event may never settle while
   * this one does.
   */
  async settlementHealth(
    watch: SettlementWatch,
    eventId: string,
    failing: string | null | undefined,
  ): Promise<string> {
    let health: Json | undefined;
    let unreadable: string | undefined;
    try {
      health = ((await this.http("/readyz", this.nodeUrl, true)).body
        .settlement ?? {}) as Json;
    } catch (error) {
      unreadable = `unreadable (${describeError(error)})`;
    }
    const state = typeof health?.state === "string" ? health.state : "unknown";
    const firstLine = (text: unknown) =>
      (typeof text === "string" ? text : "").split("\n", 1)[0]!.slice(0, 200);
    const detail = firstLine(health?.detail);
    const now = Date.now();
    const bound = this.deadlines.settlementErrorMs;
    const elapsed = (since: number) =>
      `for ${Math.round((now - since) / 1000)} s (bound ${bound / 1000} s)`;
    const awaitedError =
      state === "error" &&
      typeof health?.detail === "string" &&
      health.detail.includes(eventId);
    if (typeof failing === "string" || awaitedError) {
      const since = (watch.jobFailingSince ??= now);
      if (now - since >= bound)
        throw new FatalJourneyError(
          typeof failing === "string"
            ? `the settlement of ${eventId} has kept failing ${elapsed(since)}: ${failing}`
            : `settlement health has been 'error' for ${eventId} ${elapsed(since)}: ${detail}`,
        );
    } else if (failing === null && health !== undefined)
      delete watch.jobFailingSince;
    if (health === undefined) return unreadable!;
    // The node keeps a worker's failure streak on every report until a
    // replacement completes settlement ticks in a row, including the
    // 'starting' reports of a worker that dies waiting for the ownership lease.
    const failures = (health.workerFailures ?? {}) as Json;
    const failedRuns = typeof failures.count === "number" ? failures.count : 0;
    if (failedRuns === 0) delete watch.workerFailingSince;
    else {
      const since = (watch.workerFailingSince ??= now);
      if (now - since >= bound)
        throw new FatalJourneyError(
          `the settlement worker has kept failing (${failedRuns} runs in a row) ${elapsed(since)}: ${firstLine(failures.last)}`,
        );
    }
    return failedRuns === 0
      ? `${state}: ${detail}`
      : `${state}: ${detail} (worker failed ${failedRuns} runs in a row)`;
  }

  /**
   * Polls `check` until it returns a value. Errors are retried (a service
   * may be restarting); a FatalJourneyError ends the wait at once, and so
   * does the deadline.
   */
  async until<T>(
    what: string,
    timeoutMs: number,
    check: () => Promise<T | undefined>,
  ): Promise<T> {
    const deadline = Date.now() + timeoutMs;
    let lastError: unknown;
    let lastReport = Date.now();
    for (;;) {
      try {
        const result = await check();
        if (result !== undefined) return result;
      } catch (error) {
        if (error instanceof FatalJourneyError) throw error;
        lastError = error;
      }
      const lastProblem =
        lastError === undefined ? "" : `: ${describeError(lastError)}`;
      if (Date.now() >= deadline)
        throw new JourneyDeadlineError(
          `timed out after ${timeoutMs / 1000} s waiting for ${what}${lastProblem}`,
        );
      if (Date.now() - lastReport >= 120_000) {
        lastReport = Date.now();
        this.log(`still waiting for ${what}${lastProblem}`);
      }
      await this.wait(Math.min(this.pollMs, deadline - Date.now()));
    }
  }

  async phase(name: string, run: () => Promise<void>) {
    if (this.journal.get<string>(`phase:${name}`) === "done") {
      this.log(`phase ${name} already done`);
      return;
    }
    this.log(`phase ${name}`);
    const started = Date.now();
    await run();
    this.journal.set(`phase:${name}`, "done");
    this.log(
      `phase ${name} done in ${Math.round((Date.now() - started) / 1000)} s`,
    );
  }

  /** A journaled intent must still be what the scenario asks for. */
  protected requireSameIntent(
    what: string,
    journaled: Record<string, unknown>,
    requested: Record<string, unknown>,
  ) {
    for (const [field, value] of Object.entries(requested)) {
      const recorded = journaled[field];
      const same =
        field === "value"
          ? sameValue(toValue(recorded), toValue(value))
          : recorded === undefined || recorded === value;
      if (!same)
        throw new FatalJourneyError(
          `${what} was journaled with ${field} ${JSON.stringify(recorded)} but is now asked for ${JSON.stringify(value)}; the scenario changed under this run`,
        );
    }
  }
}
