import {
  AVAILABILITY_JOURNAL_INVENTORY_LIMITS,
  AvailabilityJournalInventoryError,
  openAvailabilityJournalInventory,
} from "@al-ft/midgard-core/availability-operation-journal-inventory";

export type AvailabilityJournalHoldsOptions = Readonly<{ journal: string }>;

/** Offline stored-row inventory. No actor credentials, provider or writer opener. */
export const runAvailabilityJournalHolds = async (
  options: AvailabilityJournalHoldsOptions,
): Promise<void> => {
  let rows = 0;
  let pages = 0;
  let inventory:
    | ReturnType<typeof openAvailabilityJournalInventory>
    | undefined;
  const cleanup = () => {
    inventory?.close();
    clearTimeout(timer);
    process.stdout.removeListener("error", outputError);
    process.removeListener("SIGINT", interrupt);
    process.removeListener("SIGTERM", interrupt);
  };
  const stop = (code: string): never => {
    process.exitCode = 1;
    // process.stdout._destroy is deliberately overridden by Node bootstrap:
    // destroy() cannot cancel its native pipe. Close the read-only snapshot and
    // terminate this CLI process; OS teardown releases its outstanding output.
    cleanup();
    try {
      process.stderr.write(
        `${JSON.stringify({ event: "availability_journal_inventory_incomplete", complete: false, code })}\n`,
      );
    } finally {
      process.exit(1);
    }
  };
  const outputError = () => {
    stop("inventory_output_failed");
  };
  const interrupt = () => {
    stop("inventory_interrupted");
  };
  const write = (event: unknown): Promise<void> =>
    new Promise((resolve) => {
      try {
        process.stdout.write(`${JSON.stringify(event)}\n`, (error) => {
          if (error) stop("inventory_output_failed");
          else resolve();
        });
      } catch {
        stop("inventory_output_failed");
      }
    });
  process.stdout.on("error", outputError);
  process.once("SIGINT", interrupt);
  process.once("SIGTERM", interrupt);
  const timer = setTimeout(() => {
    stop("inventory_lifetime_exceeded");
  }, AVAILABILITY_JOURNAL_INVENTORY_LIMITS.lifetimeMs);
  timer.unref();
  try {
    inventory = openAvailabilityJournalInventory(options.journal);
    for (;;) {
      const event = inventory.nextPage();
      await write(event);
      if (event.event === "availability_journal_inventory_complete") {
        process.exitCode = event.complete ? 0 : 1;
        break;
      }
      rows += event.rows.length;
      pages++;
    }
  } catch (error) {
    process.exitCode = 1;
    inventory?.close();
    await write({
      event: "availability_journal_inventory_complete",
      scope: "stored_journal_rows",
      authority: "stored_unreobserved",
      complete: false,
      rows,
      pages,
      code:
        error instanceof AvailabilityJournalInventoryError
          ? error.code
          : "inventory_open_failed",
    });
  } finally {
    cleanup();
  }
};
