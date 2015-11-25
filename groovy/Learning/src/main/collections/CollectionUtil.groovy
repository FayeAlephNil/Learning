package main.collections

class CollectionUtil {
	public static def listAndNega(list, negaList) {
		{
			int idx -> if (idx < 0) negaList.get(-idx) else list.get(idx)
		}
	}
}
