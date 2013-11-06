package A1;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class A1 {

	public static void main(String[] args) {

		// insertTest(); // In der Methode die Listen ändern

		List<Long> aL = new ArrayList<>();
		List<Long> lL = new LinkedList<>();
		test(aL, 4); // 1 - Einfügen, 2 - Suchen
	}

	public static void insertTest() {

		long counter = 0;
		long memory = 0;
		System.gc();
		try {
			// List<Long> L = new ArrayList<>();
			List<Long> L = new LinkedList<>();
			while (true) {
				counter++;
				memory = Runtime.getRuntime().freeMemory();
				L.add(new Long(1));
			}
		} catch (OutOfMemoryError e) {
			System.gc();
			System.out.println("Elementanzahl: " + counter);
			System.out.println("Maximaler Speicher: "
					+ Runtime.getRuntime().maxMemory());
			System.out.println("Freier Speicher: " + memory);
		}
	}

	private static void test(List<Long> L, int i) {
		long[] results = null;
		for (int s = 1; s <= 32; s *= 2) {
			switch (i) {
			case 1:
				System.out.println("Einfügen von Elementen mit " + ((s) * 3E5)
						+ " Elementen : (in ms)");
				results = insertElements(L, (s) * (long) 3E5);
				break;
			case 2:
				System.out.println("Suchen von Elementen mit " + ((s) * 3E5)
						+ " Elementen: (in ms)");
				results = searchElements(L, (s) * (long) 3E5);
				break;
			case 3:
				System.out.println("Szenario der ArrayList");
				results = szenarioArrayList(L);
				break;
			case 4:
				System.out.println("Szenario der LinkedList");
				results = szenarioLinkedList(L);
				break;
			default:
				break;
			}
			System.out.println("Maximal: " + results[0]);
			System.out.println("Minimal: " + results[1]);
			System.out.println("Durchschnitt: " + results[2]);
			System.out.println("Standardabweichung: " + results[3]);
			System.out.println();
			if (i == 3 || i == 4)
				break;
		}
	}

	private static long[] searchElements(List<Long> L, long count) {

		List<Long> measurements = new ArrayList<>();
		Long hold = new Long(count / 2);

		System.gc();
		for (long s = 0; s < count; s++) {
			L.add(new Long(s));
		}
		for (int i = 0; i < 30; i++) {
			Long start = new Long(System.currentTimeMillis());
			for (Iterator<Long> iterator = L.listIterator(); iterator.hasNext();) {
				Long long1 = (Long) iterator.next();
				if (long1.equals(hold))
					break;
			}
			Long stop = new Long(System.currentTimeMillis());
			measurements.add(stop - start);
			System.gc();
		}
		L.clear();

		return calcResults(measurements);
	}

	private static long[] insertElements(List<Long> L, long count) {

		List<Long> measurements = new ArrayList<>();
		System.gc();
		for (int i = 0; i < 30; i++) {
			Long start = new Long(System.currentTimeMillis());
			for (long s = 0; s < count; s++) {
				L.add(new Long(s));
			}
			Long stop = new Long(System.currentTimeMillis());
			L.clear();
			measurements.add(stop - start);
			System.gc();
		}
		return calcResults(measurements);
	}

	private static long[] calcResults(List<Long> measurements) {

		long[] results = { 0, 0, 0, 0 };

		for (int i = 0; i < 10; i++) {
			measurements.remove(0);
		}
		results[0] = java.util.Collections.max(measurements);
		results[1] = java.util.Collections.min(measurements);
		for (Iterator<Long> iterator = measurements.iterator(); iterator
				.hasNext();) {
			Long long1 = (Long) iterator.next();
			results[2] += long1.longValue();
		}
		results[2] = results[2] / 20;
		for (Iterator<Long> iterator = measurements.iterator(); iterator
				.hasNext();) {
			Long long1 = (Long) iterator.next();
			results[3] += Math.pow((long1.longValue() - results[2]), 2);
		}
		results[3] = (long) Math.sqrt(results[3] / 20.0);
		return results;
	}

	private static long[] szenarioArrayList(List<Long> L) {

		long length = (long) 2E4;
		List<Long> measurements = new ArrayList<>();

		System.gc();
		for (int s = 0; s < 30; s++) {
			Long start = new Long(System.currentTimeMillis());
			for (long i = 0; i < length; i++) {
				L.add(new Long(i));
			}
			for (Iterator<Long> iterator = L.listIterator(); iterator.hasNext();) {
				Long long1 = (Long) iterator.next();
				if (long1.equals(new Long(length / 2)))
					break;
			}
			for (long i = length - 1; i >= 0; i--) {
				L.remove(i);
			}
			Long stop = new Long(System.currentTimeMillis());
			L.clear();
			measurements.add(stop - start);
			System.gc();
		}
		return calcResults(measurements);
	}

	private static long[] szenarioLinkedList(List<Long> L) {

		long length = (long) 5E4;
		List<Long> measurements = new ArrayList<>();

		System.gc();
		for (int s = 0; s < 30; s++) {
			Long start = new Long(System.currentTimeMillis());
			for (long i = 0; i < length; i++) {
				L.add(0, new Long(i));
			}
			for (long i = 0; i < length; i++) {
				L.remove(0);
			}
			Long stop = new Long(System.currentTimeMillis());
			L.clear();
			measurements.add(stop - start);
			System.gc();
		}
		return calcResults(measurements);
	}
}
