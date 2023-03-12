/*******************************************************************************
 * Copyright (c) 2012 Tomas Balyo and Vojtech Bardiovsky
 * 
 * This file is part of freeLunch
 * 
 * freeLunch is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published 
 * by the Free Software Foundation, either version 3 of the License, 
 * or (at your option) any later version.
 * 
 * freeLunch is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty 
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with freeLunch.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package freelunch.core.planning.sase.api.lifted;

import java.util.HashMap;
import java.util.Map;

public class PlanningObjectRelationClass {
	
	private String name;
	private Map<String, NamedParameter> argumentClasses;
	
	public PlanningObjectRelationClass(String name, NamedParameter ... argumentClasses) {
		this.argumentClasses = new HashMap<String, NamedParameter>(argumentClasses.length);
		for (NamedParameter np : argumentClasses) {
			this.argumentClasses.put(np.getName(), np);
		}
		this.name = name;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the argumentClasses
	 */
	public Map<String, NamedParameter> getArgumentClasses() {
		return argumentClasses;
	}
	
}
