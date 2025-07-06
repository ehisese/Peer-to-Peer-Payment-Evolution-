import { describe, it, expect, beforeEach, vi } from 'vitest'

// Mock contract state to simulate Clarity contract behavior
class MockPaymentContract {
  constructor() {
    this.reset()
  }

  reset() {
    this.paymentRequests = new Map()
    this.escrowPayments = new Map()
    this.userProfiles = new Map()
    this.recurringPayments = new Map()
    this.paymentGroups = new Map()
    this.groupParticipants = new Map()
    this.transactionHistory = new Map()
    
    // Data variables
    this.paymentCounter = 0
    this.scheduleCounter = 0
    this.groupCounter = 0
    this.transactionCounter = 0
    this.platformFeeRate = 25 // 0.25%
    this.minPaymentAmount = 1000000 // 1 STX
    this.maxPaymentAmount = 1000000000000 // 1M STX
    this.contractNonce = 0
    this.contractPaused = false
    
    // Mock principals
    this.CONTRACT_OWNER = 'SP1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE'
    this.txSender = 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7'
  }

  // Utility functions
  getCurrentBlock() {
    return ++this.contractNonce
  }

  getNextPaymentId() {
    return ++this.paymentCounter
  }

  getNextScheduleId() {
    return ++this.scheduleCounter
  }

  getNextGroupId() {
    return ++this.groupCounter
  }

  getNextTransactionId() {
    return ++this.transactionCounter
  }

  calculatePlatformFee(amount) {
    return Math.floor((amount * this.platformFeeRate) / 10000)
  }

  isValidAmount(amount) {
    return amount >= this.minPaymentAmount && amount <= this.maxPaymentAmount
  }

  // Main contract functions
  createPaymentRequest(recipient, amount, memo, expiresInBlocks) {
    if (!this.isValidAmount(amount)) {
      throw new Error('ERR_INVALID_AMOUNT')
    }
    
    if (recipient === this.txSender) {
      throw new Error('ERR_INVALID_RECIPIENT')
    }

    const paymentId = this.getNextPaymentId()
    const currentBlock = this.getCurrentBlock()

    this.paymentRequests.set(paymentId, {
      sender: this.txSender,
      recipient,
      amount,
      memo,
      createdBlock: currentBlock,
      expiresBlock: currentBlock + expiresInBlocks,
      status: 'pending',
      paymentType: 'instant'
    })

    return { ok: paymentId }
  }

  payInstant(recipient, amount, memo) {
    if (!this.isValidAmount(amount)) {
      throw new Error('ERR_INVALID_AMOUNT')
    }

    if (recipient === this.txSender) {
      throw new Error('ERR_INVALID_RECIPIENT')
    }

    const platformFee = this.calculatePlatformFee(amount)
    const netAmount = amount - platformFee
    const txId = this.getNextTransactionId()

    this.transactionHistory.set(txId, {
      sender: this.txSender,
      recipient,
      amount: netAmount,
      transactionType: 'instant',
      blockNumber: this.getCurrentBlock(),
      feePaid: platformFee
    })

    return { 
      ok: { 
        paymentId: this.getNextPaymentId(), 
        netAmount, 
        transactionId: txId 
      } 
    }
  }

  createEscrowPayment(recipient, amount, arbiter, releaseCondition, disputePeriodBlocks, memo, currentBlockNum) {
    if (!this.isValidAmount(amount)) {
      throw new Error('ERR_INVALID_AMOUNT')
    }

    if (recipient === this.txSender || arbiter === this.txSender) {
      throw new Error('ERR_INVALID_RECIPIENT')
    }

    const paymentId = this.getNextPaymentId()

    this.paymentRequests.set(paymentId, {
      sender: this.txSender,
      recipient,
      amount,
      memo,
      createdBlock: currentBlockNum,
      expiresBlock: currentBlockNum + disputePeriodBlocks,
      status: 'pending',
      paymentType: 'escrow'
    })

    this.escrowPayments.set(paymentId, {
      arbiter,
      releaseCondition,
      disputeDeadline: currentBlockNum + disputePeriodBlocks,
      isDisputed: false,
      disputeReason: '',
      fundsLocked: amount
    })

    return { ok: paymentId }
  }

  setupRecurringPayment(recipient, amount, frequencyBlocks, totalPayments, startBlock) {
    if (!this.isValidAmount(amount)) {
      throw new Error('ERR_INVALID_AMOUNT')
    }

    if (recipient === this.txSender) {
      throw new Error('ERR_INVALID_RECIPIENT')
    }

    if (frequencyBlocks === 0 || totalPayments === 0) {
      throw new Error('ERR_INVALID_SCHEDULE')
    }

    const scheduleId = this.getNextScheduleId()

    this.recurringPayments.set(scheduleId, {
      payer: this.txSender,
      recipient,
      amount,
      frequencyBlocks,
      nextPaymentBlock: startBlock,
      totalPayments,
      completedPayments: 0,
      isActive: true,
      createdBlock: startBlock
    })

    return { ok: scheduleId }
  }

  createPaymentGroup(totalAmount, participantList, deadlineBlocks, currentBlockNum) {
    if (!this.isValidAmount(totalAmount)) {
      throw new Error('ERR_INVALID_AMOUNT')
    }

    if (participantList.length === 0) {
      throw new Error('ERR_INVALID_AMOUNT')
    }

    if (participantList.length > 10) {
      throw new Error('ERR_GROUP_FULL')
    }

    const groupId = this.getNextGroupId()
    const participantCount = participantList.length
    const amountPerPerson = Math.floor(totalAmount / participantCount)

    this.paymentGroups.set(groupId, {
      creator: this.txSender,
      totalAmount,
      paidAmount: 0,
      participantCount,
      deadlineBlock: currentBlockNum + deadlineBlocks,
      isCompleted: false,
      createdBlock: currentBlockNum
    })

    // Add participants
    participantList.forEach(participant => {
      this.groupParticipants.set(`${groupId}-${participant}`, {
        amountOwed: amountPerPerson,
        amountPaid: 0,
        hasPaid: false,
        paymentBlock: 0
      })
    })

    return { ok: groupId }
  }

  // Query functions
  getPaymentRequest(paymentId) {
    return this.paymentRequests.get(paymentId) || null
  }

  getEscrowPayment(paymentId) {
    return this.escrowPayments.get(paymentId) || null
  }

  getRecurringSchedule(scheduleId) {
    return this.recurringPayments.get(scheduleId) || null
  }

  getPaymentGroup(groupId) {
    return this.paymentGroups.get(groupId) || null
  }

  getGroupParticipant(groupId, participant) {
    return this.groupParticipants.get(`${groupId}-${participant}`) || null
  }

  getTransaction(txId) {
    return this.transactionHistory.get(txId) || null
  }

  getPlatformStats() {
    return {
      totalPayments: this.paymentCounter,
      totalSchedules: this.scheduleCounter,
      totalGroups: this.groupCounter,
      totalTransactions: this.transactionCounter,
      platformFeeRate: this.platformFeeRate,
      minPayment: this.minPaymentAmount,
      maxPayment: this.maxPaymentAmount,
      currentNonce: this.contractNonce
    }
  }

  // Admin functions
  updatePlatformFee(newFeeRate) {
    if (this.txSender !== this.CONTRACT_OWNER) {
      throw new Error('ERR_UNAUTHORIZED')
    }
    
    if (newFeeRate > 500) { // Max 5%
      throw new Error('ERR_INVALID_AMOUNT')
    }
    
    this.platformFeeRate = newFeeRate
    return { ok: true }
  }

  pauseContract() {
    if (this.txSender !== this.CONTRACT_OWNER) {
      throw new Error('ERR_UNAUTHORIZED')
    }
    
    this.contractPaused = true
    return { ok: true }
  }

  isContractPaused() {
    return this.contractPaused
  }
}

describe('Payment Contract Tests', () => {
  let contract
  const RECIPIENT = 'SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9'
  const ARBITER = 'SP2TPKP1QXXX1QXXX1QXXX1QXXX1QXXX1QXXX1Q'
  const AMOUNT = 5000000 // 5 STX
  const MEMO = 'Test payment'

  beforeEach(() => {
    contract = new MockPaymentContract()
  })

  describe('Basic Payment Functions', () => {
    it('should create payment request successfully', () => {
      const result = contract.createPaymentRequest(RECIPIENT, AMOUNT, MEMO, 1008)
      
      expect(result.ok).toBe(1)
      
      const payment = contract.getPaymentRequest(1)
      expect(payment.sender).toBe(contract.txSender)
      expect(payment.recipient).toBe(RECIPIENT)
      expect(payment.amount).toBe(AMOUNT)
      expect(payment.status).toBe('pending')
    })

    it('should reject invalid payment amounts', () => {
      expect(() => {
        contract.createPaymentRequest(RECIPIENT, 100, MEMO, 1008)
      }).toThrow('ERR_INVALID_AMOUNT')
    })

    it('should reject self-payments', () => {
      expect(() => {
        contract.createPaymentRequest(contract.txSender, AMOUNT, MEMO, 1008)
      }).toThrow('ERR_INVALID_RECIPIENT')
    })

    it('should execute instant payment', () => {
      const result = contract.payInstant(RECIPIENT, AMOUNT, MEMO)
      
      expect(result.ok.netAmount).toBe(AMOUNT - Math.floor((AMOUNT * 25) / 10000))
      expect(result.ok.transactionId).toBe(1)
      
      const tx = contract.getTransaction(1)
      expect(tx.sender).toBe(contract.txSender)
      expect(tx.recipient).toBe(RECIPIENT)
      expect(tx.transactionType).toBe('instant')
    })
  })

  describe('Escrow Payment Functions', () => {
    it('should create escrow payment', () => {
      const result = contract.createEscrowPayment(
        RECIPIENT, 
        AMOUNT, 
        ARBITER, 
        'Delivery confirmation', 
        1008, 
        MEMO, 
        100
      )
      
      expect(result.ok).toBe(1)
      
      const payment = contract.getPaymentRequest(1)
      expect(payment.paymentType).toBe('escrow')
      
      const escrow = contract.getEscrowPayment(1)
      expect(escrow.arbiter).toBe(ARBITER)
      expect(escrow.fundsLocked).toBe(AMOUNT)
      expect(escrow.isDisputed).toBe(false)
    })

    it('should reject escrow with invalid arbiter', () => {
      expect(() => {
        contract.createEscrowPayment(
          RECIPIENT, 
          AMOUNT, 
          contract.txSender, 
          'Delivery confirmation', 
          1008, 
          MEMO, 
          100
        )
      }).toThrow('ERR_INVALID_RECIPIENT')
    })
  })

  describe('Recurring Payment Functions', () => {
    it('should setup recurring payment', () => {
      const result = contract.setupRecurringPayment(
        RECIPIENT, 
        AMOUNT, 
        144, // Daily
        30, // 30 payments
        100
      )
      
      expect(result.ok).toBe(1)
      
      const schedule = contract.getRecurringSchedule(1)
      expect(schedule.payer).toBe(contract.txSender)
      expect(schedule.recipient).toBe(RECIPIENT)
      expect(schedule.totalPayments).toBe(30)
      expect(schedule.completedPayments).toBe(0)
      expect(schedule.isActive).toBe(true)
    })

    it('should reject invalid schedule parameters', () => {
      expect(() => {
        contract.setupRecurringPayment(RECIPIENT, AMOUNT, 0, 30, 100)
      }).toThrow('ERR_INVALID_SCHEDULE')
      
      expect(() => {
        contract.setupRecurringPayment(RECIPIENT, AMOUNT, 144, 0, 100)
      }).toThrow('ERR_INVALID_SCHEDULE')
    })
  })

  describe('Group Payment Functions', () => {
    it('should create payment group', () => {
      const participants = [
        'SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9',
        'SP2TPKP1QXXX1QXXX1QXXX1QXXX1QXXX1QXXX1Q',
        'SP1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE'
      ]
      
      const result = contract.createPaymentGroup(
        15000000, // 15 STX total
        participants,
        1008, // 1 week deadline
        100
      )
      
      expect(result.ok).toBe(1)
      
      const group = contract.getPaymentGroup(1)
      expect(group.creator).toBe(contract.txSender)
      expect(group.totalAmount).toBe(15000000)
      expect(group.participantCount).toBe(3)
      expect(group.isCompleted).toBe(false)
      
      // Check participants
      const participant1 = contract.getGroupParticipant(1, participants[0])
      expect(participant1.amountOwed).toBe(5000000) // 15M / 3
      expect(participant1.hasPaid).toBe(false)
    })

    it('should reject group with too many participants', () => {
      const participants = Array(11).fill().map((_, i) => `SP${i}`)
      
      expect(() => {
        contract.createPaymentGroup(AMOUNT, participants, 1008, 100)
      }).toThrow('ERR_GROUP_FULL')
    })
  })

  describe('Query Functions', () => {
    beforeEach(() => {
      contract.createPaymentRequest(RECIPIENT, AMOUNT, MEMO, 1008)
      contract.payInstant(RECIPIENT, AMOUNT, MEMO)
    })

    it('should get payment request details', () => {
      const payment = contract.getPaymentRequest(1)
      
      expect(payment).not.toBeNull()
      expect(payment.sender).toBe(contract.txSender)
      expect(payment.recipient).toBe(RECIPIENT)
      expect(payment.amount).toBe(AMOUNT)
    })

    it('should get transaction details', () => {
      const tx = contract.getTransaction(1)
      
      expect(tx).not.toBeNull()
      expect(tx.sender).toBe(contract.txSender)
      expect(tx.recipient).toBe(RECIPIENT)
      expect(tx.transactionType).toBe('instant')
    })

    it('should get platform statistics', () => {
      const stats = contract.getPlatformStats()
      
      expect(stats.totalPayments).toBe(2) // createPaymentRequest + payInstant
      expect(stats.totalTransactions).toBe(1) // payInstant
      expect(stats.platformFeeRate).toBe(25)
      expect(stats.minPayment).toBe(1000000)
    })
  })

  describe('Admin Functions', () => {
    it('should update platform fee by owner', () => {
      contract.txSender = contract.CONTRACT_OWNER
      
      const result = contract.updatePlatformFee(50) // 0.5%
      
      expect(result.ok).toBe(true)
      expect(contract.platformFeeRate).toBe(50)
    })

    it('should reject fee update by non-owner', () => {
      expect(() => {
        contract.updatePlatformFee(50)
      }).toThrow('ERR_UNAUTHORIZED')
    })

    it('should reject excessive fee rates', () => {
      contract.txSender = contract.CONTRACT_OWNER
      
      expect(() => {
        contract.updatePlatformFee(1000) // 10%
      }).toThrow('ERR_INVALID_AMOUNT')
    })

    it('should pause contract by owner', () => {
      contract.txSender = contract.CONTRACT_OWNER
      
      const result = contract.pauseContract()
      
      expect(result.ok).toBe(true)
      expect(contract.isContractPaused()).toBe(true)
    })

    it('should reject pause by non-owner', () => {
      expect(() => {
        contract.pauseContract()
      }).toThrow('ERR_UNAUTHORIZED')
    })
  })

  describe('Edge Cases', () => {
    it('should handle null queries gracefully', () => {
      expect(contract.getPaymentRequest(999)).toBeNull()
      expect(contract.getEscrowPayment(999)).toBeNull()
      expect(contract.getRecurringSchedule(999)).toBeNull()
    })

    it('should calculate platform fees correctly', () => {
      const fee = contract.calculatePlatformFee(1000000) // 1 STX
      expect(fee).toBe(2500) // 0.25% of 1 STX
    })

    it('should validate amount ranges', () => {
      expect(contract.isValidAmount(1000000)).toBe(true) // Min amount
      expect(contract.isValidAmount(999999)).toBe(false) // Below min
      expect(contract.isValidAmount(1000000000000)).toBe(true) // Max amount
      expect(contract.isValidAmount(1000000000001)).toBe(false) // Above max
    })
  })
})