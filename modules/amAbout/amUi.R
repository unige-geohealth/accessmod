#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

tagList(
  tags$div(
    class = "col-sm-8",
    tags$div(
      class = "col-sm-12 well",
      h4(amt("about_license")),
      p(amt("about_license_text")),
      hr(),
      h4(amt("about_acknowledge")),
      p(amt("about_acknowledge_text")),
      hr(),
      h4(amt("help_learn_accessmod")),
      p(
        tags$b("English"),
        a(
          href = "https://doc-accessmod.unepgrid.ch/display/EN/AccessMod+5+user+manual",
          target = "_blank",
          "Access to online user manual"
        )
      ),
      p(
        tags$b("Français"),
        a(
          href = "https://doc-accessmod.unepgrid.ch/display/FRAN/AccessMod+5+manuel+de+l%27utilisateur",
          target = "_blank",
          "Accès en ligne au manuel de l'utilisateur"
        )
      )
    )
  )
)
