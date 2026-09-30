package main

import (
	"bytes"
	"encoding/hex"
	"errors"
	"fmt"
	"github.com/blinklabs-io/gouroboros/ledger"
	"github.com/blinklabs-io/gouroboros/protocol/chainsync"
	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"net"
	"time"
)

type queryLimitedConn struct {
	net.Conn
	remaining int
	deadline  time.Time
}

// The muxer refreshes its segment deadline before every read. It must never
// extend the exact query's absolute operation deadline.
func (c *queryLimitedConn) SetReadDeadline(deadline time.Time) error {
	if !c.deadline.IsZero() && (deadline.IsZero() || deadline.After(c.deadline)) {
		deadline = c.deadline
	}
	return c.Conn.SetReadDeadline(deadline)
}

// The pinned muxer arms a segment deadline before reading its header. NtC may
// legitimately wait indefinitely between segments (AwaitReply or owner
// backpressure). Start that deadline at the first byte instead, retaining the
// bounded read of the remaining header and payload. Only the muxer's single
// reader calls Read and SetReadDeadline; Close still interrupts an idle read.
type streamSegmentConn struct {
	net.Conn
	segmentTimeout  time.Duration
	awaitingSegment bool
}

func (c *streamSegmentConn) SetReadDeadline(deadline time.Time) error {
	c.awaitingSegment = !deadline.IsZero()
	c.segmentTimeout = time.Until(deadline)
	return c.Conn.SetReadDeadline(time.Time{})
}

func (c *streamSegmentConn) Read(buffer []byte) (int, error) {
	if !c.awaitingSegment || len(buffer) == 0 {
		return c.Conn.Read(buffer)
	}
	n, err := c.Conn.Read(buffer[:1])
	if n > 0 {
		c.awaitingSegment = false
		if deadlineErr := c.Conn.SetReadDeadline(time.Now().Add(c.segmentTimeout)); deadlineErr != nil && err == nil {
			err = deadlineErr
		}
	}
	return n, err
}

func (c *queryLimitedConn) Read(buffer []byte) (int, error) {
	if c.remaining == 0 {
		return 0, errors.New("native exact-query ingress bound exceeded")
	}
	if len(buffer) > c.remaining {
		buffer = buffer[:c.remaining]
	}
	n, err := c.Conn.Read(buffer)
	c.remaining -= n
	return n, err
}

type exactQueryState struct {
	config       startupConfig
	events       int
	acknowledged bool
	captured     bool
}

func (q *exactQueryState) checkBackward(point pcommon.Point) error {
	if q.config.Operation.Kind != "exact_point" {
		return nil
	}
	q.events++
	expected, err := pointFromStartup(q.config.Intersection)
	if err != nil {
		return err
	}
	if q.captured || q.acknowledged || q.events > 2 || point.Slot != expected.Slot || !bytes.Equal(point.Hash, expected.Hash) {
		return errors.New("native exact-query rolled back outside initial intersection acknowledgement")
	}
	q.acknowledged = true
	return nil
}

func (q *exactQueryState) checkForward(block ledger.Block) error {
	if q.config.Operation.Kind != "exact_point" {
		return nil
	}
	q.events++
	target := q.config.Operation.Target
	if q.captured || q.events > 2 || target == nil ||
		block.Hash().String() != target.BlockHash ||
		fmt.Sprintf("%d", block.SlotNumber()) != target.Slot ||
		fmt.Sprintf("%d", block.BlockNumber()) != target.BlockNo ||
		block.PrevHash().String() != q.config.Intersection.BlockHash {
		return errors.New("native exact-query returned a different target or an extra block")
	}
	q.captured = true
	return nil
}

func makeChainSyncConfig(config startupConfig, writer *canonicalWriter, readyGate <-chan struct{}) chainsync.Config {
	query := exactQueryState{config: config}
	result := chainsync.Config{
		PipelineLimit: 1,
		RecvQueueSize: 4,
		RollForwardRawFunc: func(_ chainsync.CallbackContext, blockType uint, raw []byte, eventTip chainsync.Tip) error {
			<-readyGate
			if len(raw) == 0 || len(raw) > maxBlockBytes {
				return errors.New("native chain-sync block size is invalid")
			}
			block, err := ledger.NewBlockFromCbor(blockType, raw)
			if err != nil {
				return fmt.Errorf("decode native chain-sync block: %w", err)
			}
			if err := query.checkForward(block); err != nil {
				return err
			}
			prevHash := block.PrevHash().String()
			if block.BlockNumber() == 0 && prevHash == fmt.Sprintf("%064d", 0) {
				prevHash = ""
			}
			if err := writer.write(rollForwardEvent{
				BlockHash:     block.Hash().String(),
				BlockNo:       fmt.Sprintf("%d", block.BlockNumber()),
				BlockType:     fmt.Sprintf("%d", blockType),
				Kind:          "roll_forward",
				PrevHash:      prevHash,
				RawBlockCBOR:  hex.EncodeToString(raw),
				SchemaVersion: schemaVersion,
				Slot:          fmt.Sprintf("%d", block.SlotNumber()),
				Tip:           tip(eventTip),
			}); err != nil {
				return err
			}
			if config.Operation.Kind == "exact_point" {
				// PipelineLimit=1 leaves no later request outstanding. Returning
				// false through ErrStopSyncProcess stops the client's syncLoop;
				// it does not suspend a callback or close this connection.
				return chainsync.ErrStopSyncProcess
			}
			return nil
		},
		RollBackwardFunc: func(_ chainsync.CallbackContext, point pcommon.Point, eventTip chainsync.Tip) error {
			<-readyGate
			if err := query.checkBackward(point); err != nil {
				return err
			}
			rollbackPoint := wirePoint{Kind: "origin"}
			if len(point.Hash) > 0 || point.Slot != 0 {
				rollbackPoint = wirePoint{
					BlockHash: hex.EncodeToString(point.Hash),
					Kind:      "point",
					Slot:      fmt.Sprintf("%d", point.Slot),
				}
			}
			return writer.write(rollBackwardEvent{
				Kind:          "roll_backward",
				Point:         rollbackPoint,
				SchemaVersion: schemaVersion,
				Tip:           tip(eventTip),
			})
		},
	}
	if config.Operation.Kind == "exact_point" {
		result.RecvQueueSize = 1
	}
	return result
}
