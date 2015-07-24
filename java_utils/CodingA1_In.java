//quick sort pseudocode for partition
partion (A, l, r);
pivot = A[l];
i = l + 1;
for (j=l+1;j<=r;j++){
	if (A[j] < p){
		//swap
		tmp = A[j];
		A[j] = A[i];
		A[i] = tmp;
		i++;
	}
}
//at last swap the pivot
A[l] = A[i-1];
A[i-1] = p;

// In CA1 solution
/* first common Ancestor for binary tree */
public interface firstCommonAncestor {
	/**
	 * given two nodes of a tree,
	 * method return the deepest common ancestor of those nodes.
	 */
		Node commonAncestor(Node one, Node two);
}

class Node {
	final Node parent;
	final Node left;
	final Node right;
	
	public Node(Node parent, Node left, Node right){
		this.parent = parent;
		this.left = left;
		this.right = right;
	}
	
	boolean isRoot(){
		return parent==null;
	}
}

//solution 1

class firstCommonAncestorSolution1 implements firstCommonAncestor {
	public Node commonAncestor(Node a, Node b){
		int depthA = depth(a);
		int depthB = depth(b);
		int delta = Math.abs(depthA-depthB);
		//compute which is deeper, make the deeper one the first
		Node first = depthA>depthB?a:b;
		Node second = depthA>depthB?b:a;
		
		//move the first one up to the same level as the second
		for(int i=0; i < delta; i++){
			first = first.parent;
		}
		
		//now the first and the second are in the same level
		//so moving up 1 level at a time, will eventually result in a refereing the same node
		//root node is the worst case
		
		
		
	}
}


// In CA1 solutions
// 2015-07-18

public List <String> Paginate (String input){
	//Break the input up to words. We define a world to be a sequence of non-whitespace characters
	List<String> words = Arrays.asList(input.split("\\s"));
	Iterator<String> wordItr = words.iterator();
	List<String> output = new ArrayList <String>();
	List<String> page = new ArrayList <String>();
	int currentLength = 0;
	//Walk the list, assembling pages
	while (wordItr.hasNext()){
		String nextWord = wordItr.next();
		int speculateLength = currentLength + nextWord.length();
		if (speculateLength < lineLength){
			//the word fits, add to the current page
			page.add(nextWord);
			currentLength = speculateLength + 1; //include the space between it and the next word
			// this may put the current page over length (which is fine)
			continue;
		}
		else{
			// The word does not fit on this page. Process and reset the line
			output.add(addSpaces(page));
			page = new ArrayList <String> ();
			currentLength = 0;
			page.add(nextWord);
			currentLength = nextWord.length() + 1;
			
		}
			
	}
	//we run out of words. Turn whatever left into the last page
	if(!page.isEmpty()){
		output.append(addSpaces(page));
	}
	
	return(output);
}

// if we need to justify the page such that we have exact length (by adding space)

private String addSpaces(List<String> words, int lineLength){
	//build the string
	StringBuilder output = new StringBuilder();
	for (int i = 0; i < words.size() - 1; i++){
		//we do not want to appy this to the last words
		String word = words.get(i);
		output.append(word);
		output.append(" ");
	}
	// append the last word
	output.append(words.get(words.size()-1));
	return output.toString();
}


/*100++ games*/
// no resuable case, first to achieve 100 wins
/* write a procedure, Boolean canIWin(int maxChoosableInteger, int desiredTotal),
which returns true if the first player to move can force a win with optimal play.

your priority shoudl be programmer efficiencey, don't focus on minimizing either
space or time complexity
*/

boolean canIWin(int maxChoosableInteger, int desiredTotal) throws Exception {
	boolean[] numPool = new boolean[maxChoosableInteger + 1]; //initialized to be False
	// use numPool[i] to indicate if num i is availabe or not (0 is out of the picture)
	// if numPool[i] == True, i is already used
	int maxAchievable = (1 + maxChoosableInteger)*maxChoosableInteger/2;
	if (maxAchievable <= desiredTotal) {
		throw new Exception('Game canno be won!');
	}
	
	return canIWin(0, desiredTotal, numPool)
}

boolean canIWin(int currentTotal, int desiredTotal, boolean [] usedNums){
	if(currentTotal >= desiredTotal)
		return False; // the opponent already won
	for(int i = 1; i <= usedNums.length - 1; i++){
		if (usedNum[i])
			continue; // Num was used before
		usedNum[i] = true; // mark i as used and try see if it can leads to a win
		boolean opponentCanWin = canIWin(currentTotal + i, desiredTotal, usedNum);
		usedNum[i] = false; //put i back and try another
		// if found a play where my opponent can't win, then I have won
		if (!opponentCanWin)
			return true;
	}
	return false; 
	//if for all possible numbers to choose you can not find a number that
	//can make your opponent to lose, you lose
}