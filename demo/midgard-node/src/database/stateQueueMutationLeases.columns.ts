/** Table shape of `state_queue_mutation_leases`, shared by the lease
 * modules. */
export const tableName = "state_queue_mutation_leases";

export const SCOPE = "state_queue";
export const DEFAULT_TTL_MS = 10 * 60 * 1000;
export const DEFAULT_RENEW_INTERVAL_MS = 60 * 1000;

export const Status = {
  Active: "active",
  Released: "released",
  Failed: "failed",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export enum Columns {
  TOKEN = "token",
  SCOPE = "scope",
  HOLDER = "holder",
  STATUS = "status",
  ACQUIRED_AT = "acquired_at",
  EXPIRES_AT = "expires_at",
  RELEASED_AT = "released_at",
  LAST_ERROR = "last_error",
}

export type Entry = {
  [Columns.TOKEN]: string;
  [Columns.SCOPE]: string;
  [Columns.HOLDER]: string;
  [Columns.STATUS]: Status;
  [Columns.ACQUIRED_AT]: Date;
  [Columns.EXPIRES_AT]: Date;
  [Columns.RELEASED_AT]: Date | null;
  [Columns.LAST_ERROR]: string | null;
};
