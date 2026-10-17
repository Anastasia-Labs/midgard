/**
 * What `fit:regenerate --check` compares between a checked-in fit ledger and
 * the one a fresh run of its owners writes: every field, the execution
 * budgets and their margins included, except the two digests. Owners run on
 * fixed wallet seeds and a fixed emulator clock, so a fresh run of one
 * blueprint measures the same budgets. `blueprintSha256` and `ledgerSha256`
 * (whose body holds the blueprint digest) name the build and the file's
 * bytes, not what was measured, so a blueprint change makes a ledger stale
 * only when its numbers move. A field only one side has is a difference, so
 * a ledger still carrying a field the format dropped is stale.
 */
const UNCOMPARED_LEDGER_FIELDS = new Set(["blueprintSha256", "ledgerSha256"]);

const shown = (value) =>
  value === undefined ? "absent" : JSON.stringify(value);

const fieldDifferences = (label, recorded, fresh, uncompared = new Set()) =>
  [...new Set([...Object.keys(recorded), ...Object.keys(fresh)])]
    .filter(
      (field) =>
        !uncompared.has(field) &&
        JSON.stringify(recorded[field]) !== JSON.stringify(fresh[field]),
    )
    .map(
      (field) =>
        `${label}${field}: recorded ${shown(recorded[field])}, fresh ${shown(fresh[field])}`,
    );

const rowsOf = (ledger, side, differences) => {
  if (Array.isArray(ledger.entries)) return ledger.entries;
  differences.push(`entries: the ${side} ledger has no row list`);
  return [];
};

/** Each way the recorded ledger differs from the fresh one, as one line. */
export const fitLedgerDifferences = (recorded, fresh) => {
  const differences = [];
  const { entries: _recordedRows, ...recordedLedger } = recorded;
  const { entries: _freshRows, ...freshLedger } = fresh;
  differences.push(
    ...fieldDifferences(
      "",
      recordedLedger,
      freshLedger,
      UNCOMPARED_LEDGER_FIELDS,
    ),
  );
  const recordedRows = rowsOf(recorded, "recorded", differences);
  const freshRows = rowsOf(fresh, "fresh", differences);
  const freshByName = new Map(freshRows.map((row) => [row.name, row]));
  const recordedNames = new Set(recordedRows.map(({ name }) => name));
  for (const row of recordedRows) {
    const freshRow = freshByName.get(row.name);
    if (freshRow === undefined)
      differences.push(`row ${row.name}: recorded, not in the fresh run`);
    else
      differences.push(...fieldDifferences(`row ${row.name} `, row, freshRow));
  }
  for (const { name } of freshRows)
    if (!recordedNames.has(name))
      differences.push(`row ${name}: in the fresh run, not recorded`);
  const sequence = (rows) => JSON.stringify(rows.map(({ name }) => name));
  if (
    recordedNames.size === freshByName.size &&
    [...recordedNames].every((name) => freshByName.has(name)) &&
    sequence(recordedRows) !== sequence(freshRows)
  )
    differences.push("rows: recorded in another order or more than once");
  return differences;
};
