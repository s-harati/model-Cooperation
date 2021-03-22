package cooptest17;

public class Registrar {

	private AgentG agentG;
	private AgentU[] agentUArray;
	private int nLast, nSum, agentUCount; // are we using agentUCount here? yes: in update :/ // shall we calculate densities here too
	
	public Registrar(AgentG agentG, AgentU[] agentUArray, int agentUCount) {
		this.agentG = agentG;
		this.agentUArray = agentUArray;
		this.agentUCount = agentUCount;
		this.resetStats();
	}
	
	public int getNLast() {
		return this.nLast;
	}
	
	public int getNSum() {
		return this.nSum;
	}
	
	// this function is used in Exploring Starts algorithm
	public void setNLast(int startingNLast) {
		this.nLast = startingNLast;
	}
	
	// this function is used in Exploring Starts algorithm
	public void setNSum(int startingNSum) {
		this.nSum = startingNSum;
	}
	
	public void resetStats() {
		this.nLast = 0; this.nSum = 0;
		System.out.println("*in Registrar.resetStats() . new nLast="+this.nLast+"  new nSum="+this.nSum+" *");
		for (AgentU agentU : agentUArray) {
			agentU.setDecision(0);
		}
	}
	
	public void step() {
		if (agentG.getSignal() == 0) {
			nSum += agentUCount;
		} else {
			// debugging
			//System.out.println("in registrar, before calling agentUs nLast:" + nLast);
						
			for (AgentU agentU : agentUArray) {
				agentU.makeDecision();
			}
			this.nLast = 0;
			for (AgentU agentU : agentUArray) {
				nLast += agentU.getDecision();
			}
			// debugging
			//System.out.println("in registrar, after calling agentUs nLast:" + nLast);
			nSum += nLast;
		}
	}
	

}
