package main

import (
	"errors"
	"fmt"
	"time"

	gcbor "github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/localstatequery"
	"github.com/blinklabs-io/gouroboros/protocol/localtxsubmission"
)

// Query tags of the N2C LocalStateQuery codec (ouroboros-consensus).
const (
	queryBlock        = 0
	querySystemStart  = 1
	queryChainBlockNo = 2
	queryChainPoint   = 3

	blockQueryShelley  = 0
	blockQueryHardFork = 2

	hardForkEraHistory = 0
	hardForkCurrentEra = 1

	shelleyCurrentProtocolParams = 3
	shelleyUtxoByAddress         = 6
	shelleyFilteredDelegations   = 10
	shelleyUtxoByTxIn            = 15
	shelleyStakeDelegDeposits    = 22

	maxQueryItems = 4096
)

// boundedClient runs one request/reply mini-protocol under per-request
// deadlines. A request whose reply misses its deadline is answered with
// node_timeout and the session lives on; the reply is still owed, and it is
// taken before the next request is sent, within that request's deadline.
// A protocol never has more than one reply owed: nothing is sent while one
// is.
type boundedClient struct {
	raw  *rawProtocol
	node *nodeConn
	owed bool
}

func errNodeTimeout(what string) *requestError {
	return refuse("node_timeout", "%s within the request deadline", what)
}

// await waits for the reply to the message just sent.
func (c *boundedClient) await(deadline time.Time) (protocol.Message, error) {
	timer := time.NewTimer(time.Until(deadline))
	defer timer.Stop()
	select {
	case msg := <-c.raw.messages:
		return msg, nil
	case <-c.node.dead:
		return nil, c.node.deathCause()
	case <-timer.C:
		c.owed = true
		return nil, errNodeTimeout("the node did not answer")
	}
}

// begin takes a reply still owed to an abandoned request, handing it to
// late, and refuses a request whose deadline passed while it was queued.
func (c *boundedClient) begin(deadline time.Time, late func(protocol.Message)) error {
	if c.owed {
		timer := time.NewTimer(time.Until(deadline))
		defer timer.Stop()
		select {
		case msg := <-c.raw.messages:
			c.owed = false
			late(msg)
		case <-c.node.dead:
			return c.node.deathCause()
		case <-timer.C:
			return errNodeTimeout("the node did not answer an earlier request")
		}
	}
	if !time.Now().Before(deadline) {
		return errNodeTimeout("the request waited behind earlier requests and was not sent")
	}
	return nil
}

// lsqClient drives LocalStateQuery and returns each answer's raw bytes.
// It is used by one goroutine at a time (the session's LSQ worker).
type lsqClient struct {
	boundedClient
	acquired bool
}

// settle takes a reply owed to an abandoned acquire or query: a late
// acquisition or failure changes what is acquired; a late query result
// leaves it.
func (c *lsqClient) settle(deadline time.Time) error {
	return c.begin(deadline, func(msg protocol.Message) {
		switch msg.(type) {
		case *localstatequery.MsgAcquired:
			c.acquired = true
		case *localstatequery.MsgFailure:
			c.acquired = false
		}
	})
}

// acquire acquires point, or the volatile tip when point is nil. An
// acquisition replaces the previous one (ReAcquire).
func (c *lsqClient) acquire(point *wirePoint, deadline time.Time) error {
	if err := c.settle(deadline); err != nil {
		return err
	}
	var err error
	switch {
	case point == nil && c.acquired:
		err = c.raw.send(localstatequery.NewMsgReAcquireVolatileTip())
	case point == nil:
		err = c.raw.send(localstatequery.NewMsgAcquireVolatileTip())
	case c.acquired:
		err = c.raw.send(localstatequery.NewMsgReAcquire(point.common()))
	default:
		err = c.raw.send(localstatequery.NewMsgAcquire(point.common()))
	}
	if err != nil {
		return err
	}
	msg, err := c.await(deadline)
	if err != nil {
		return err
	}
	switch reply := msg.(type) {
	case *localstatequery.MsgAcquired:
		c.acquired = true
		return nil
	case *localstatequery.MsgFailure:
		c.acquired = false
		switch reply.Failure {
		case localstatequery.AcquireFailurePointTooOld:
			return refuse("acquire_point_too_old", "the point is older than the node's volatile window")
		case localstatequery.AcquireFailurePointNotOnChain:
			return refuse("acquire_point_not_on_chain", "the point is not on the node's chain")
		default:
			return refuse("acquire_failed", "acquire failure %d", reply.Failure)
		}
	default:
		return fmt.Errorf("unexpected reply %d to Acquire", msg.Type())
	}
}

func (c *lsqClient) release(deadline time.Time) error {
	if err := c.settle(deadline); err != nil {
		return err
	}
	if !c.acquired {
		return nil
	}
	c.acquired = false
	return c.raw.send(localstatequery.NewMsgRelease())
}

func (c *lsqClient) query(query any, deadline time.Time) ([]byte, error) {
	if err := c.settle(deadline); err != nil {
		return nil, err
	}
	if !c.acquired {
		return nil, refuse("not_acquired", "acquire a ledger state before querying")
	}
	if err := c.raw.send(localstatequery.NewMsgQuery(query)); err != nil {
		return nil, err
	}
	msg, err := c.await(deadline)
	if err != nil {
		return nil, err
	}
	result, ok := msg.(*localstatequery.MsgResult)
	if !ok {
		return nil, errors.New("unexpected reply to Query")
	}
	return append([]byte(nil), result.Result...), nil
}

// currentEra is re-read per query: an acquisition may cross an era.
func (c *lsqClient) currentEra(deadline time.Time) (uint64, error) {
	raw, err := c.query([]any{queryBlock, []any{blockQueryHardFork, []any{hardForkCurrentEra}}}, deadline)
	if err != nil {
		return 0, err
	}
	var era uint64
	if _, err := gcbor.Decode(raw, &era); err != nil {
		return 0, fmt.Errorf("current era answer: %w", err)
	}
	return era, nil
}

// eraQuery runs one Shelley-based query in the current era and removes the
// hard-fork combinator's era-match wrapper: [answer] is a match, a
// two-element array is an era mismatch.
func (c *lsqClient) eraQuery(deadline time.Time, tag uint64, params ...any) ([]byte, error) {
	era, err := c.currentEra(deadline)
	if err != nil {
		return nil, err
	}
	inner := append([]any{tag}, params...)
	raw, err := c.query([]any{queryBlock, []any{blockQueryShelley, []any{era, inner}}}, deadline)
	if err != nil {
		return nil, err
	}
	var wrapper []gcbor.RawMessage
	if _, err := gcbor.Decode(raw, &wrapper); err != nil {
		return nil, fmt.Errorf("era query answer: %w", err)
	}
	if len(wrapper) != 1 {
		return nil, refuse("era_mismatch", "the ledger answered for a different era")
	}
	return append([]byte(nil), wrapper[0]...), nil
}

func (c *lsqClient) run(header lsqQueryHeader, deadline time.Time) ([]byte, error) {
	if len(header.Addresses) > maxQueryItems || len(header.TxIns) > maxQueryItems || len(header.Credentials) > maxQueryItems {
		return nil, refuse("invalid_request", "query parameter list exceeds %d items", maxQueryItems)
	}
	only := func(addresses, txIns, credentials bool) error {
		if (!addresses && header.Addresses != nil) || (!txIns && header.TxIns != nil) || (!credentials && header.Credentials != nil) {
			return refuse("invalid_request", "query %s does not take these parameters", header.Query)
		}
		return nil
	}
	switch header.Query {
	case "system_start":
		if err := only(false, false, false); err != nil {
			return nil, err
		}
		return c.query([]any{querySystemStart}, deadline)
	case "chain_block_no":
		if err := only(false, false, false); err != nil {
			return nil, err
		}
		return c.query([]any{queryChainBlockNo}, deadline)
	case "chain_point":
		if err := only(false, false, false); err != nil {
			return nil, err
		}
		return c.query([]any{queryChainPoint}, deadline)
	case "current_era":
		if err := only(false, false, false); err != nil {
			return nil, err
		}
		return c.query([]any{queryBlock, []any{blockQueryHardFork, []any{hardForkCurrentEra}}}, deadline)
	case "era_history":
		if err := only(false, false, false); err != nil {
			return nil, err
		}
		return c.query([]any{queryBlock, []any{blockQueryHardFork, []any{hardForkEraHistory}}}, deadline)
	case "protocol_params":
		if err := only(false, false, false); err != nil {
			return nil, err
		}
		return c.eraQuery(deadline, shelleyCurrentProtocolParams)
	case "utxo_by_address":
		if err := only(true, false, false); err != nil {
			return nil, err
		}
		if len(header.Addresses) == 0 {
			return nil, refuse("invalid_request", "utxo_by_address needs at least one address")
		}
		// Encoded as gouroboros' own client does: addresses and inputs as plain
		// arrays, credentials as a tagged set.
		return c.eraQuery(deadline, shelleyUtxoByAddress, header.Addresses)
	case "utxo_by_txin":
		if err := only(false, true, false); err != nil {
			return nil, err
		}
		if len(header.TxIns) == 0 {
			return nil, refuse("invalid_request", "utxo_by_txin needs at least one input")
		}
		for _, input := range header.TxIns {
			if len(input.TxID) != 32 {
				return nil, refuse("invalid_request", "transaction id is not 32 bytes")
			}
		}
		return c.eraQuery(deadline, shelleyUtxoByTxIn, header.TxIns)
	case "stake_deleg_deposits", "filtered_delegations_and_rewards":
		if err := only(false, false, true); err != nil {
			return nil, err
		}
		if len(header.Credentials) == 0 {
			return nil, refuse("invalid_request", "%s needs at least one credential", header.Query)
		}
		for _, credential := range header.Credentials {
			if credential.Tag > 1 || len(credential.Hash) != 28 {
				return nil, refuse("invalid_request", "credential is not [0|1, hash28]")
			}
		}
		tag := uint64(shelleyStakeDelegDeposits)
		if header.Query == "filtered_delegations_and_rewards" {
			tag = shelleyFilteredDelegations
		}
		return c.eraQuery(deadline, tag, gcbor.NewSetType(header.Credentials, true))
	default:
		return nil, refuse("unknown_query", "unknown query %q", header.Query)
	}
}

// submitClient drives LocalTxSubmission and returns the node's raw
// rejection bytes; it never interprets them. A submission answered with
// node_timeout has an unknown outcome: the node may still accept it.
type submitClient struct {
	boundedClient
}

func (c *submitClient) submit(era uint16, tx []byte, deadline time.Time) (accepted bool, reason []byte, err error) {
	if err := c.begin(deadline, func(protocol.Message) {}); err != nil {
		return false, nil, err
	}
	if err := c.raw.send(localtxsubmission.NewMsgSubmitTx(era, tx)); err != nil {
		return false, nil, err
	}
	msg, err := c.await(deadline)
	if err != nil {
		return false, nil, err
	}
	switch reply := msg.(type) {
	case *localtxsubmission.MsgAcceptTx:
		return true, nil, nil
	case *localtxsubmission.MsgRejectTx:
		return false, append([]byte(nil), reply.Reason...), nil
	default:
		return false, nil, errors.New("unexpected reply to SubmitTx")
	}
}
